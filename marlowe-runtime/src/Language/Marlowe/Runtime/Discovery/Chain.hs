{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Discovery.Chain
  where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard)
import Data.Crosswalk (crosswalk)
import Data.Foldable (asum, fold, for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (ContractId(..), MarloweVersion(..), SomeMarloweVersion(..), fromChainDatum, withSomeMarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), getMarloweVersion, getScripts)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Network.Protocol.Driver (RunClient)
import qualified Plutus.V1.Ledger.Api as P

data Changes = Changes
  { headers :: !(Map Chain.BlockHeader (Set ContractHeader))
  , rollbackTo :: !(Maybe Chain.ChainPoint)
  } deriving (Show, Eq)

instance Semigroup Changes where
  c1 <> Changes{..} = c1' { headers = Map.unionWith (<>) headers1 headers }
    where
      c1'@Changes{headers=headers1} = maybe c1 (flip applyRollback c1) rollbackTo

instance Monoid Changes where
  mempty = Changes Map.empty Nothing

isEmptyChanges :: Changes -> Bool
isEmptyChanges (Changes headers Nothing) = null $ fold headers
isEmptyChanges _ = False

applyRollback :: Chain.ChainPoint -> Changes -> Changes
applyRollback Chain.Genesis _ = Changes mempty $ Just Chain.Genesis
applyRollback (Chain.At blockHeader@Chain.BlockHeader{slotNo}) Changes{..} = Changes
  { headers = headers'
  , rollbackTo = asum
      [ guard (Map.null headers') *> (min (Just (Chain.At blockHeader)) rollbackTo <|> Just (Chain.At blockHeader))
      , rollbackTo
      ]
  }
  where
    headers' = Map.filterWithKey (const . isNotRolledBack) headers
    isNotRolledBack = not . Chain.isAfter slotNo

newtype DiscoveryChainClientDependencies = DiscoveryChainClientDependencies
  { connectToChainSeek :: RunClient IO Chain.RuntimeChainSeekClient
  }

data DiscoveryChainClient = DiscoveryChainClient
  { runDiscoveryChainClient :: IO Void
  , changes :: STM Changes
  }

mkDiscoveryChainClient :: DiscoveryChainClientDependencies -> STM DiscoveryChainClient
mkDiscoveryChainClient DiscoveryChainClientDependencies{..} = do
  changesVar <- newTVar mempty
  let
    clientInit = Chain.SendMsgRequestHandshake Chain.moveSchema clientHandshake
    clientHandshake = Chain.ClientStHandshake
      { recvMsgHandshakeRejected = \versions -> error
          $ "ChainSeek handshake failed. Requested schema version "
          <> show Chain.moveSchema
          <> ", requires "
          <> show versions
          <> "."
      , recvMsgHandshakeConfirmed = pure clientIdle
      }

    clientIdle = Chain.SendMsgQueryNext
      (Chain.FindTxsTo $ Set.map Chain.ScriptCredential marloweScriptHashes)
      clientNext
      (pure clientNext)

    clientNext = Chain.ClientStNext
      { recvMsgQueryRejected = \_ _ -> pure clientIdle
      , recvMsgRollForward = \txs -> \case
          Chain.Genesis -> error "Roll forward to Genesis"
          Chain.At block -> \_ -> do
            atomically $ for_ (fmap fold $ crosswalk (extractHeaders block) $ Set.toList txs) \headers ->
              modifyTVar changesVar (<> mempty { headers = Map.singleton block headers})
            pure clientIdle
      , recvMsgRollBackward = \point _ -> do
          atomically $ modifyTVar changesVar (<> mempty { rollbackTo = Just point })
          pure clientIdle
      }

  pure DiscoveryChainClient
    { runDiscoveryChainClient = connectToChainSeek $ Chain.ChainSeekClient $ pure clientInit
    , changes = do
        changes <- readTVar changesVar
        writeTVar changesVar mempty
        pure changes
    }

extractHeaders :: Chain.BlockHeader -> Chain.Transaction -> Maybe (Set ContractHeader)
extractHeaders blockHeader Chain.Transaction{..} =
  Set.fromList <$> crosswalk (uncurry extractHeader) (zip [0..] outputs)
  where
    isMarloweAddress address = case Chain.paymentCredential address of
      Just (Chain.ScriptCredential hash) -> Set.member hash marloweScriptHashes
      _ -> False
    isMarloweInput Chain.TransactionInput{address} = isMarloweAddress address
    hasMarloweInput = any isMarloweInput inputs
    extractHeader ix Chain.TransactionOutput{..} = do
      guard $ not hasMarloweInput
      credential <- Chain.paymentCredential address
      marloweScriptHash <- case credential of
        Chain.ScriptCredential hash -> pure hash
        _ -> Nothing
      (SomeMarloweVersion version, MarloweScripts{..}) <- getMarloweVersion marloweScriptHash
      marloweDatum <- fromChainDatum version =<< datum
      let
        rolesCurrency = case version of
          MarloweV1 -> Chain.PolicyId
            $ P.fromBuiltin
            $ P.unCurrencySymbol
            $ V1.rolesCurrency
            $ V1.marloweParams marloweDatum
      pure ContractHeader
        { contractId = ContractId $ Chain.TxOutRef txId $ fromInteger ix
        , rolesCurrency
        , metadata = mempty -- TODO we need to index metadata in chain sync!
        , marloweScriptHash
        , marloweScriptAddress = address
        , payoutScriptHash = payoutScript
        , marloweVersion = SomeMarloweVersion version
        , blockHeader
        }

marloweScriptHashes :: Set Chain.ScriptHash
marloweScriptHashes = Set.map marloweScript $ foldMap (withSomeMarloweVersion getScripts) [minBound..maxBound]
