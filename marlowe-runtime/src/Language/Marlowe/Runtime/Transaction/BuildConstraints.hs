{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraints
  ( buildApplyInputsConstraints
  , buildCreateConstraints
  , buildWithdrawConstraints
  ) where

import Cardano.Api (CardanoMode, EraHistory(..))
import qualified Cardano.Api.Byron as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.BaseTypes as CL (Network(..))
import Control.Arrow (Arrow((***)))
import Control.Category ((>>>))
import Control.Error (note)
import Control.Monad ((<=<), (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Data.Bifunctor (bimap, first)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (find, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import GHC.Base (Alternative((<|>)))
import GHC.Natural (Natural)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Runtime.Cardano.Api (plutusScriptHash, toCardanoAddressAny, toCardanoPlutusScript)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(Address)
  , AssetId(AssetId)
  , Assets(Assets)
  , Lovelace(Lovelace)
  , Metadata(MetadataBytes, MetadataMap)
  , PaymentKeyHash(PaymentKeyHash)
  , PolicyId(PolicyId)
  , ScriptHash(unScriptHash)
  , TokenName(unTokenName)
  , TransactionMetadata(TransactionMetadata, unTransactionMetadata)
  , TransactionOutput(TransactionOutput)
  , UTxO(UTxO)
  , toUTxOsList
  )
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , IsMarloweVersion(Datum)
  , MarloweVersion(MarloweV1)
  , MarloweVersionTag(V1)
  , PayoutDatum
  , Redeemer
  , TransactionScriptOutput(TransactionScriptOutput)
  , withMarloweVersion
  )
import Language.Marlowe.Runtime.Plutus.V2.Api
  ( fromPlutusCurrencySymbol
  , fromPlutusScript
  , fromPlutusTokenName
  , toPlutusAddress
  , toPlutusCurrencySymbol
  , toPlutusTokenName
  , toPlutusTxOutRef
  )
import qualified Language.Marlowe.Runtime.Plutus.V2.Scripts.MarloweV1.RoleTokensPolicy as RoleTokensPolicy
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsConstraintsBuildupError(..)
  , ApplyInputsError(..)
  , CreateBuildupError(AddressDecodingFailed, MintingScriptDecodingFailed, MintingUtxoSelectionFailed)
  , CreateError(..)
  , Mint(unMint)
  , NFTMetadata(unNFTMetadata)
  , WithdrawError
  )
import Language.Marlowe.Runtime.Transaction.Constraints
  ( TxConstraints(..)
  , WalletContext(WalletContext)
  , mustConsumeMarloweOutput
  , mustMintRoleToken
  , mustPayToAddress
  , mustPayToRole
  , mustSendMarloweOutput
  , mustSendMerkleizedContinuationOutput
  , mustSpendRoleToken
  , requiresMetadata
  , requiresSignature
  )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Ouroboros.Consensus.BlockchainTime (SystemStart, fromRelativeTime, toRelativeTime)
import Ouroboros.Consensus.HardFork.History (interpretQuery, slotToWallclock, wallclockToSlot)
import qualified Ouroboros.Network.Block as O
import qualified Plutus.V2.Ledger.Api as P
import qualified Plutus.V2.Ledger.Api as PV2
import qualified PlutusTx.AssocMap as AM

maxFees :: Lovelace
maxFees = Lovelace 2_170_000

-- FIXME: This is arbitrary value - adjust this better.
minAdaPerTokenOutput :: Lovelace
minAdaPerTokenOutput  = Lovelace 10_000

type TxConstraintsBuilderM err v a = WriterT (TxConstraints v) (Either err) a

execTxConstraintsBuilder :: MarloweVersion v -> TxConstraintsBuilderM err v a -> Either err (TxConstraints v)
execTxConstraintsBuilder v = execWriterT . withMarloweVersion v

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraints
  :: forall v
   . MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> WalletContext  -- ^ The wallet used to mint tokens.
  -> Maybe (Either PolicyId Mint) -- ^ The initial distribution of the role tokens.
  -> TransactionMetadata -- ^ Extra metadata to add to the transaction.
  -> Lovelace -- ^ Min Lovelace value which should be used on the Marlowe output.
  -> Contract v -- ^ The contract being instantiated.
  -> Either (CreateError v) (TxConstraints v)
buildCreateConstraints version walletCtx roles metadata minAda contract = case version of
  MarloweV1 -> execTxConstraintsBuilder version $ buildCreateConstraintsV1 walletCtx roles metadata minAda contract

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraintsV1
  :: WalletContext  -- ^ The wallet used to mint tokens.
  -> Maybe (Either PolicyId Mint) -- ^ The initial distribution of the role tokens.
  -> TransactionMetadata -- ^ Extra metadata to add to the transaction.
  -> Lovelace -- ^ Min Lovelace value which should be used on the Marlowe output.
  -> Contract 'V1 -- ^ The contract being instantiated.
  -> TxConstraintsBuilderM (CreateError 'V1) 'V1 ()
buildCreateConstraintsV1 walletCtx roles metadata minAda contract = do
  -- Tx body constraints.
  let
    metadata' = metadata <> nftsMetadata
  for_ (Map.toList . unTransactionMetadata $ metadata') \(label, meta) -> do
    tell . requiresMetadata label $ meta

  -- Output constraints.
  -- Role tokens minting and distribution.
  policyId <- mintRoleTokens

  -- Marlowe script output.
  sendMarloweOutput policyId
  where
    nftsMetadata = case roles of
      Just (Right (Map.toList . unMint -> minting)) -> do
        let
          tokensMetadata = catMaybes $ minting <&> \case
            (tokenName, (_, Right (Just (unNFTMetadata -> nftMetadata)))) -> do
              let
                tokenName' = unTokenName tokenName
              -- From CIP-25: In version 2 the the raw bytes of the asset_name are used.
              Just (MetadataBytes tokenName', nftMetadata)
            _ -> Nothing
        case tokensMetadata of
          [] -> mempty
          metadata' -> TransactionMetadata (Map.singleton 721 (MetadataMap metadata'))
      _ -> mempty

    liftMaybe err = lift . note (CreateBuildupFailed err)

    sendMarloweOutput policyId = do
      datum <- mkMarloweDatum policyId
      tell $ mustSendMarloweOutput (adaAsset minAda) datum

    mkMarloweDatum :: PolicyId -> TxConstraintsBuilderM (CreateError 'V1) 'V1 (Datum 'V1)
    mkMarloweDatum policyId = do
      marloweState <- mkInitialMarloweState
      let
        marloweParams = V1.MarloweParams . toPlutusCurrencySymbol $ policyId
      pure $ V1.MarloweData marloweParams marloweState contract

    mkInitialMarloweState :: TxConstraintsBuilderM (CreateError 'V1) 'V1 V1.State
    mkInitialMarloweState = do
      let
        WalletContext { changeAddress=minAdaProvider } = walletCtx
      (net, addr) <- liftMaybe (AddressDecodingFailed minAdaProvider) do
        address <- toPlutusAddress minAdaProvider
        network <- toMarloweNetwork minAdaProvider
        pure (network, address)
      let
        accountId = V1.Address net addr
        adaToken = V1.Token PV2.adaSymbol PV2.adaToken
        initialAccounts :: V1.Accounts
        initialAccounts = AM.singleton (accountId, adaToken) (toInteger minAda)
      pure (V1.emptyState (PV2.POSIXTime 0)) { V1.accounts = initialAccounts }

    toMarloweNetwork :: Address -> Maybe V1.Network
    toMarloweNetwork = toCardanoAddressAny >=> \case
        C.AddressByron (C.ByronAddress _) -> Nothing
        C.AddressShelley (C.ShelleyAddress CL.Mainnet _ _) -> Just V1.mainnet
        C.AddressShelley _ -> Just V1.testnet

    adaAsset :: Lovelace -> Assets
    adaAsset amount = Assets amount mempty

    -- Role token distribution constraints
    mintRoleTokens :: TxConstraintsBuilderM (CreateError 'V1) 'V1 PolicyId
    mintRoleTokens = case roles of
      Just (Left policyId) -> pure policyId
      Just (Right (unMint -> minting)) -> do
        let
          WalletContext { availableUtxos } = walletCtx
          txLovelaceRequirementEstimate = adaAsset $
            minAda
            + maxFees
            + Lovelace (fromInteger . toInteger . length $ minting) * minAdaPerTokenOutput
          utxoAssets UTxO {transactionOutput = TransactionOutput { assets }} = assets
          possibleInput = (find ((<) txLovelaceRequirementEstimate . utxoAssets) . sortBy (compare `on` utxoAssets) . toUTxOsList $ availableUtxos)
            <|> listToMaybe (toUTxOsList availableUtxos)

        UTxO txOutRef _ <- liftMaybe MintingUtxoSelectionFailed possibleInput
        let
          txOutRef' = toPlutusTxOutRef txOutRef

          tokenAmount :: (Address, Either Natural (Maybe NFTMetadata)) -> Integer
          tokenAmount (_, Left amount) = toInteger amount
          tokenAmount (_, Right _) = 1

          roleTokens = RoleTokensPolicy.mkRoleTokens (map (toPlutusTokenName *** tokenAmount) . Map.toList $ minting)
          plutusScript = fromPlutusScript . PV2.getMintingPolicy . RoleTokensPolicy.policy roleTokens $ txOutRef'

        (script, scriptHash) <- liftMaybe (MintingScriptDecodingFailed plutusScript) do
          script <- toCardanoPlutusScript plutusScript
          scriptHash <- plutusScriptHash plutusScript
          pure (script, scriptHash)
        let
          witness = C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            (C.PScript script)
            C.NoScriptDatumForMint
            (C.fromPlutusData $ PV2.toData RoleTokensPolicy.Mint)
            (C.ExecutionUnits 0 0)
          policyId = PolicyId . unScriptHash $ scriptHash

        for_ (Map.toList minting) \(tokenName, (address, _)) ->
          tell $ mustMintRoleToken txOutRef witness (AssetId policyId tokenName) address
        pure policyId
      Nothing -> do
        let
          -- We use ADA currency symbol as a placeholder which
          -- carries really no semantics in this context.
          uselessRolePolicyId  = PolicyId . PV2.fromBuiltin . PV2.unCurrencySymbol $ PV2.adaSymbol
        pure uselessRolePolicyId

-- applies an input to a contract.
buildApplyInputsConstraints
  :: SystemStart
  -> EraHistory CardanoMode -- ^ The era history for converting times to slots.
  -> MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> TransactionScriptOutput v -- ^ The previous script output for the contract
  -> UTCTime -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime -- ^ The maximum bound of the validity interval (exclusive).
                   -- If not specified, this is computed from the the timeouts
                   -- in the contract.
  -> Redeemer v -- ^ The inputs to apply to the contract.
  -> Either (ApplyInputsError v) (TxConstraints v)
buildApplyInputsConstraints systemStart eraHistory version marloweOutput invalidBefore invalidHereafter redeemer =
  case version of
    MarloweV1 -> buildApplyInputsConstraintsV1 systemStart eraHistory marloweOutput invalidBefore invalidHereafter redeemer

-- | Creates a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraintsV1
  :: SystemStart
  -> EraHistory CardanoMode -- ^ The era history for converting times to slots.
  -> TransactionScriptOutput 'V1 -- ^ The previous script output for the contract with raw TxOut.
  -> UTCTime -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime -- ^ The maximum bound of the validity interval (exclusive).
  -> Redeemer 'V1 -- ^ The inputs to apply to the contract.
  -> Either (ApplyInputsError 'V1) (TxConstraints 'V1)
buildApplyInputsConstraintsV1 systemStart eraHistory marloweOutput invalidBefore invalidHereafter redeemer = execWriterT do
  let
    TransactionScriptOutput _ _ _ datum = marloweOutput
    V1.MarloweData params state contract = datum
    V1.MarloweParams currencySymbol = params

    requiredParties = Set.fromList $ for redeemer $ marloweInputParty >>> maybeToList
    roleAssetId = toAssetId currencySymbol

  invalidBefore' <- lift $ utcTimeToSlotNo invalidBefore

  invalidHereafter' <- lift $ do
    ib <- note (ApplyInputsConstraintsBuildupFailed UnableToDetermineTransactionTimeout) $ invalidHereafter <|> nextMarloweTimeout contract
    utcTimeToSlotNo ib

  -- Construct inputs constraints.
  -- Consume UTXOs containing Marlowe script.
  tell $ mustConsumeMarloweOutput invalidBefore' invalidHereafter' redeemer

  -- Consume UTXOs containing all necessary role tokens and send them back.
  for_ requiredParties $ traverse_ $ \case
    V1.Role role -> tell $ mustSpendRoleToken $ roleAssetId role
    _ -> pure ()

  -- Require signature of an every party which is authorized through an address.
  for_ requiredParties $ traverse_ $ \case
    V1.Address _ address -> case address of
      P.Address (P.PubKeyCredential (P.PubKeyHash pkh)) _ ->
        tell $ requiresSignature $ PaymentKeyHash $ P.fromBuiltin pkh
      _ -> pure ()
    _ -> pure ()

  -- Apply inputs.
  let slotNoToPOSIXTime = fmap utcToPOSIXTime . slotStart
  txInterval <- lift $ (,) <$> slotNoToPOSIXTime invalidBefore' <*> slotNoToPOSIXTime invalidHereafter'
  let transactionInput = V1.TransactionInput { txInterval, txInputs = redeemer }
  (possibleContinuation, payments) <- case V1.computeTransaction transactionInput state contract of
     V1.Error err -> lift $ Left $ ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed $ show err)
     V1.TransactionOutput _ payments _ V1.Close ->
       pure (Nothing, payments)
     V1.TransactionOutput _ payments state' contract' ->
       pure (Just (state', contract'), payments)

  -- Construct outputs constraints.
  -- Require Marlowe output if the contract is not closed.
  for_ possibleContinuation \(state'@V1.State { accounts }, contract') -> do
      let
        datum' = V1.MarloweData params state' contract'
        assets = moneyToAssets $ V1.totalBalance accounts
      tell $ mustSendMarloweOutput assets datum'

  -- For every payment require an output either to the role
  -- payout script or directly to the party address.
  for_ payments \(V1.Payment _ payee token quantity) -> do
    let
      assets = case token of
        V1.Token "" "" -> Assets (Lovelace $ fromInteger quantity) mempty
        V1.Token cs tn -> do
          let
            assetId = toAssetId cs tn
            quantity' = fromInteger quantity
          Assets (Lovelace 0) (CS.Tokens $ Map.singleton assetId quantity')
    case payee of
      V1.Party (V1.Address net addr) -> do
        let addr' = Address $ V1.serialiseAddress net addr
        tell $ mustPayToAddress assets addr'
      V1.Party (V1.Role role) ->
        tell $ mustPayToRole assets $ roleAssetId role
      V1.Account _ -> pure ()

  -- For every merkleized input require an output which contains the continuation in the datum.
  for_ redeemer \input -> do
    case marloweMerkleizedContinuation input of
      Just cont -> tell $ mustSendMerkleizedContinuationOutput cont
      Nothing -> pure ()

  where
    marloweMerkleizedContinuation (V1.NormalInput _) = Nothing
    marloweMerkleizedContinuation (V1.MerkleizedInput _ _ c) = Just c

    marloweInputContent (V1.NormalInput c) = c
    marloweInputContent (V1.MerkleizedInput c _ _) = c

    marloweInputParty = marloweInputContent >>> \case
      V1.IDeposit _ party _ _         -> Just party
      V1.IChoice (V1.ChoiceId _ party) _ -> Just party
      V1.INotify                      -> Nothing

    moneyToAssets :: V1.Money -> Assets
    moneyToAssets = Assets <$> moneyToLovelace <*> moneyToTokens

    moneyToLovelace :: V1.Money -> Lovelace
    moneyToLovelace = Lovelace . maybe 0 fromIntegral . (AM.lookup "" <=< AM.lookup "") . P.getValue

    toAssetId cs role = do
      let
        policyId = fromPlutusCurrencySymbol cs
        tokenName = fromPlutusTokenName role
      CS.AssetId policyId tokenName

    moneyToTokens :: V1.Money -> CS.Tokens
    moneyToTokens = CS.Tokens
      . Map.fromList
      . fmap
          ( bimap (uncurry toAssetId) fromIntegral
          . assocLeft
          )
      . (traverse AM.toList <=< AM.toList)
      . AM.delete ""
      . P.getValue

    assocLeft (a, (b, c)) = ((a, b), c)

    EraHistory _ interpreter = eraHistory

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo :: UTCTime -> Either (ApplyInputsError 'V1) C.SlotNo
    utcTimeToSlotNo time = do
      let relativeTime = toRelativeTime systemStart time
      (slotNo, _, _) <- first (SlotConversionFailed . show)
        $ interpretQuery interpreter
        $ wallclockToSlot relativeTime
      pure $ C.SlotNo $ O.unSlotNo slotNo

    slotStart :: C.SlotNo -> Either (ApplyInputsError 'V1) UTCTime
    slotStart (C.SlotNo slotNo) = do
      (relativeTime, _) <- first (SlotConversionFailed . show)
        $ interpretQuery interpreter
        $ slotToWallclock
        $ O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

    utcToPOSIXTime :: UTCTime -> PV2.POSIXTime
    utcToPOSIXTime = PV2.POSIXTime . floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

    posixTimeToUTCTime :: PV2.POSIXTime -> UTCTime
    posixTimeToUTCTime (P.POSIXTime t) = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromInteger t / 1000

    nextMarloweTimeout :: V1.Contract -> Maybe UTCTime
    nextMarloweTimeout (V1.When _ timeout _) = Just $ posixTimeToUTCTime timeout
    nextMarloweTimeout _ = Nothing


-- | Creates a set of Tx constraints that are used to build a transaction that
-- withdraws payments from a payout validator.
buildWithdrawConstraints
  :: MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> PayoutDatum v -- ^ The role token from which to withdraw funds.
  -> Either (WithdrawError v) (TxConstraints v)
buildWithdrawConstraints = \case
  MarloweV1 -> Right . buildWithdrawConstraintsV1
  where
    buildWithdrawConstraintsV1 :: AssetId -> TxConstraints 'V1
    buildWithdrawConstraintsV1 =
      TxConstraints.mustConsumePayouts <> TxConstraints.mustSpendRoleToken
