{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.QueryServer
  where

import Cardano.Api
  ( AnyCardanoEra(..)
  , CardanoMode
  , ConsensusMode(..)
  , ConsensusModeIsMultiEra(..)
  , EraInMode(..)
  , GenesisParameters(..)
  , QueryInEra(..)
  , QueryInMode(..)
  , QueryInShelleyBasedEra(..)
  , ShelleyBasedEra(..)
  , toEraInMode
  )
import qualified Cardano.Api as Cardano
import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Except (ExceptT(ExceptT), except, runExceptT, throwE, withExceptT)
import Data.Bifunctor (bimap, first)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..), SlotConfig(..))
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Query.Server (QueryServer(..), ServerStInit(..), ServerStNext(..), ServerStPage(..))
import Network.Protocol.Query.Types (StNextKind(..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import System.IO (hPutStrLn, stderr)
import Unsafe.Coerce (unsafeCoerce)

type RunQueryServer m = RunServer m (QueryServer ChainSyncQuery)

data ChainSyncQueryServerDependencies = ChainSyncQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> IO (Either AcquireFailure result)
  , getUTxOs :: !(Database.GetUTxOs IO)
  }

newtype ChainSyncQueryServer = ChainSyncQueryServer
  { runChainSyncQueryServer :: IO Void
  }

mkChainSyncQueryServer :: ChainSyncQueryServerDependencies -> STM ChainSyncQueryServer
mkChainSyncQueryServer ChainSyncQueryServerDependencies{..} = do
  let
    runChainSyncQueryServer = do
      runQueryServer <- acceptRunQueryServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runChainSyncQueryServer
  pure $ ChainSyncQueryServer { runChainSyncQueryServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Query worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runQueryServer      :: RunQueryServer IO
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> IO (Either AcquireFailure result)
  , getUTxOs :: !(Database.GetUTxOs IO)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunServer run = runQueryServer
  in
    pure Worker { runWorker = run server }

  where
    server :: QueryServer ChainSyncQuery IO ()
    server = QueryServer $ pure $ ServerStInit \case
      GetSlotConfig        -> queryGenesisParameters extractSlotConfig
      GetSecurityParameter -> queryGenesisParameters protocolParamSecurity
      GetNetworkId -> queryGenesisParameters protocolParamNetworkId
      GetProtocolParameters -> toServerStNext <$> queryShelley (const QueryProtocolParameters)
      GetSystemStart ->
        toServerStNext . bimap (const ()) unsafeCoerce <$> queryLocalNodeState Nothing QuerySystemStart
      GetEraHistory ->
        toServerStNext . first (const ()) <$> queryLocalNodeState Nothing (QueryEraHistory CardanoModeIsMultiEra)
      GetUTxOs utxosQuery -> do
        utxos <- Database.runGetUTxOs getUTxOs utxosQuery
        pure $ toServerStNext $ Right utxos

    toServerStNext :: Either () a -> ServerStNext ChainSyncQuery 'CanReject Void () a IO ()
    toServerStNext = \case
      Left _ -> SendMsgReject () ()
      Right a -> SendMsgNextPage a Nothing $ ServerStPage
        { recvMsgDone = pure ()
        , recvMsgRequestNext = absurd
        }

    queryGenesisParameters :: (GenesisParameters -> a) -> IO (ServerStNext ChainSyncQuery 'CanReject Void () a IO ())
    queryGenesisParameters f = toServerStNext . fmap f <$> queryShelley (const QueryGenesisParameters)

    queryShelley
      :: (forall era. ShelleyBasedEra era -> QueryInShelleyBasedEra era result)
      -> IO (Either () result)
    queryShelley query = runExceptT do
      AnyCardanoEra era <- withExceptT (const ())
        $ ExceptT
        $ queryLocalNodeState Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra
      eraInMode <- case toEraInMode era CardanoMode of
        Nothing        -> throwE ()
        Just eraInMode -> pure eraInMode
      shelleyBasedEra <- case eraInMode of
        ByronEraInCardanoMode   -> throwE ()
        ShelleyEraInCardanoMode -> pure ShelleyBasedEraShelley
        AllegraEraInCardanoMode -> pure ShelleyBasedEraAllegra
        MaryEraInCardanoMode    -> pure ShelleyBasedEraMary
        AlonzoEraInCardanoMode  -> pure ShelleyBasedEraAlonzo
        BabbageEraInCardanoMode -> pure ShelleyBasedEraBabbage
      result <- withExceptT (const ())
        $ ExceptT
        $ queryLocalNodeState Nothing
        $ QueryInEra eraInMode
        $ QueryInShelleyBasedEra shelleyBasedEra $ query shelleyBasedEra
      withExceptT (const ()) $ except result

    extractSlotConfig GenesisParameters{..} = SlotConfig protocolParamSystemStart protocolParamSlotLength
