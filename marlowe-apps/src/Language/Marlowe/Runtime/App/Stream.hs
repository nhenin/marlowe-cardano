

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , hasClosed
  , streamAllContractIds
  , streamAllContractSteps
  , streamContractHeaders
  , streamContractSteps
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Maybe (isNothing, mapMaybe)
import Data.Type.Equality ((:~:)(Refl))
import Language.Marlowe.Runtime.App.Run (runMarloweHeaderSyncClient, runMarloweSyncClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId
  , IsMarloweVersion(..)
  , MarloweVersion
  , MarloweVersionTag(V1)
  , Transaction(Transaction, output)
  , TransactionOutput(TransactionOutput, scriptOutput)
  , assertVersionsEqual
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep(ApplyTransaction), CreateStep)

import qualified Language.Marlowe.Protocol.HeaderSync.Client as HS
  ( ClientStIdle(SendMsgRequestNext)
  , ClientStNext(..)
  , ClientStWait(SendMsgPoll)
  , MarloweHeaderSyncClient(MarloweHeaderSyncClient)
  )
import qualified Language.Marlowe.Protocol.Sync.Client as CS
  ( ClientStFollow(ClientStFollow, recvMsgContractFound, recvMsgContractNotFound)
  , ClientStIdle(SendMsgDone, SendMsgRequestNext)
  , ClientStInit(SendMsgFollowContract)
  , ClientStNext(..)
  , ClientStWait(SendMsgPoll)
  , MarloweSyncClient(MarloweSyncClient)
  )


streamAllContractIds
  :: TChan ContractId
  -> Client (Either String ())
streamAllContractIds = streamContractHeaders $ Just . contractId


streamContractHeaders
  :: (ContractHeader -> Maybe a)
  -> TChan a
  -> Client (Either String ())
streamContractHeaders extract channel =
  let
    clientIdle = HS.SendMsgRequestNext clientNext
    clientWait = HS.SendMsgPoll clientNext
    clientNext =
      HS.ClientStNext
      {
        HS.recvMsgNewHeaders = \_ results -> do
                                                 liftIO . atomically
                                                   . mapM_ (writeTChan channel)
                                                   $ mapMaybe extract results
                                                 pure clientIdle
      , HS.recvMsgRollBackward = const $ pure clientIdle
      , HS.recvMsgWait = clientWait <$ liftIO (threadDelay 5_000_000)
      }
  in
    runMarloweHeaderSyncClient runDiscoverySyncClient
      . HS.MarloweHeaderSyncClient
      $ pure clientIdle


data ContractStream v =
    ContractStreamStart
    {
      csContractId :: ContractId
    , csBlockHeader :: BlockHeader
    , csCreateStep :: CreateStep v
    }
  | ContractStreamContinued
    {
      csContractId :: ContractId
    , csBlockHeader :: BlockHeader
    , csContractStep :: ContractStep v
    }
  | ContractStreamRolledBack
    {
      csContractId :: ContractId
    , csBlockHeader :: BlockHeader
    }
  | ContractStreamFinish
    {
      csContractId :: ContractId
    , csFinish :: Either String Bool  -- ^ Either an error message or an indication whether the contract closed.
    }

instance ToJSON (ContractStream 'V1) where
  toJSON ContractStreamStart{..} =
    object
      [
        "contractId" .= csContractId
      , "blockHeader" .= csBlockHeader
      , "createStep" .= csCreateStep
      ]
  toJSON ContractStreamContinued{..} =
    object
      [
        "contractId" .= csContractId
      , "blockHeader" .= csBlockHeader
      , "contractStep" .= csContractStep
      ]
  toJSON ContractStreamRolledBack{..} =
    object
      [
        "contractId" .= csContractId
      , "blockHeader" .= csBlockHeader
      ]
  toJSON ContractStreamFinish{..} =
    object
      [
        "contractId" .= csContractId
      , "finish" .= csFinish
      ]


streamAllContractSteps
  :: forall v
  .  IsMarloweVersion v
  => ContractId
  -> TChan (ContractStream v)
  -> Client ()
streamAllContractSteps = streamContractSteps True $ const True


hasClosed :: ContractStep v -> Bool
hasClosed (ApplyTransaction Transaction{output=TransactionOutput{..}}) = isNothing scriptOutput
hasClosed  _ = False


streamContractSteps
  :: forall v
  .  IsMarloweVersion v
  => Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChan (ContractStream v)
  -> Client ()
streamContractSteps finishOnClose accept csContractId channel =
  let
    clientInit =
      CS.SendMsgFollowContract csContractId
        CS.ClientStFollow
        {
          CS.recvMsgContractNotFound = liftIO . atomically
                                         . writeTChan channel
                                         . ContractStreamFinish csContractId
                                         $ Left "Contract not found."
        , CS.recvMsgContractFound = \csBlockHeader version csCreateStep ->
            case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
              Refl -> if accept $ Left csCreateStep
                        then do
                               liftIO . atomically
                                 $ writeTChan channel
                                   ContractStreamStart{..}
                               pure $ clientIdle version
                        else do
                               liftIO . atomically
                                 . writeTChan channel
                                 . ContractStreamFinish csContractId
                                 $ Right False
                               pure $ CS.SendMsgDone ()
        }
    clientIdle = CS.SendMsgRequestNext . clientNext
    clientWait = CS.SendMsgPoll . clientNext
    clientNext :: MarloweVersion v -> CS.ClientStNext v Client ()
    clientNext version =
      CS.ClientStNext
      {
        CS.recvMsgRollBackCreation =
          liftIO . atomically
            . writeTChan channel
            . ContractStreamFinish csContractId
            $ Left "Creation transaction was rolled back."
      , CS.recvMsgRollBackward = \csBlockHeader -> do
          liftIO . atomically
            $ writeTChan channel
              ContractStreamRolledBack{..}
          pure $ clientIdle version
      , CS.recvMsgRollForward = \csBlockHeader steps -> do
          let
            acceptances = accept . Right <$> steps
          liftIO . atomically
            . mapM_ (\csContractStep -> writeTChan channel ContractStreamContinued{..})
            $ fst <$> takeWhile snd (zip steps acceptances)
          if and acceptances
            then if finishOnClose && any hasClosed steps
                   then do
                          liftIO . atomically
                            . writeTChan channel
                            . ContractStreamFinish csContractId
                            $ Right True
                          pure $ CS.SendMsgDone ()
                   else pure $ clientIdle version
            else do
                   liftIO . atomically
                     . writeTChan channel
                     . ContractStreamFinish csContractId
                     $ Right False
                   pure $ CS.SendMsgDone ()
      , CS.recvMsgWait = clientWait version <$ liftIO (threadDelay 5_000_000)
      }
  in
    runMarloweSyncClient runHistorySyncClient
      . CS.MarloweSyncClient
      $ pure clientInit
