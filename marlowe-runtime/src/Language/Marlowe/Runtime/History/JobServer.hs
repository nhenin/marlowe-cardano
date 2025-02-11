{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History.JobServer
  where

import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (SomeException, catch)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Job.Server (liftCommandHandler)
import System.IO (hPutStrLn, stderr)

type RunJobServer m = RunServer m RuntimeHistoryJobServer

data HistoryJobServerDependencies = HistoryJobServerDependencies
  { acceptRunJobServer    :: IO (RunJobServer IO)
  , followContract        :: ContractId -> STM Bool
  , stopFollowingContract :: ContractId -> STM Bool
  , followerStatuses      :: STM (Map ContractId FollowerStatus)
  }

newtype HistoryJobServer = HistoryJobServer
  { runHistoryJobServer :: IO ()
  }

mkHistoryJobServer :: HistoryJobServerDependencies -> STM HistoryJobServer
mkHistoryJobServer HistoryJobServerDependencies{..} = do
  let
    runHistoryJobServer = do
      runJobServer <- acceptRunJobServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runHistoryJobServer
  pure $ HistoryJobServer { runHistoryJobServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Job worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runJobServer          :: RunJobServer IO
  , followContract        :: ContractId -> STM Bool
  , stopFollowingContract :: ContractId -> STM Bool
  , followerStatuses      :: STM (Map ContractId FollowerStatus)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunServer run = runJobServer
  in
    pure Worker { runWorker = run server }

  where
    server :: RuntimeHistoryJobServer IO ()
    server = liftCommandHandler $ fmap ((),) . \case
      Left (FollowContract contractId) -> do
        followed <- atomically $ followContract contractId
        if followed
          then atomically do
            statuses <- followerStatuses
            case Map.lookup contractId statuses of
              Nothing           -> retry
              Just Pending      -> retry
              Just (Failed err) -> pure $ Left err
              Just _            -> pure $ Right True
          else pure $ Right False
      Left (StopFollowingContract contractId) -> atomically $ Right <$> stopFollowingContract contractId
      Right jobId -> case jobId of
