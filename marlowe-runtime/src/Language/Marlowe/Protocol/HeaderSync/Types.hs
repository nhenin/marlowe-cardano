{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.HeaderSync.Types
  where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol (Protocol(..))

data MarloweHeaderSync where
  StIdle :: MarloweHeaderSync
  StIntersect :: MarloweHeaderSync
  StNext :: MarloweHeaderSync
  StWait :: MarloweHeaderSync
  StDone :: MarloweHeaderSync

instance Protocol MarloweHeaderSync where
  data Message MarloweHeaderSync from to where
    MsgIntersect :: [BlockHeader] -> Message MarloweHeaderSync
      'StIdle
      'StIntersect
    MsgDone :: Message MarloweHeaderSync
      'StIdle
      'StDone
    MsgRequestNext :: Message MarloweHeaderSync
      'StIdle
      'StNext
    MsgNewHeaders :: BlockHeader -> [ContractHeader] -> Message MarloweHeaderSync
      'StNext
      'StIdle
    MsgRollBackward :: ChainPoint -> Message MarloweHeaderSync
      'StNext
      'StIdle
    MsgWait :: Message MarloweHeaderSync
      'StNext
      'StWait
    MsgPoll :: Message MarloweHeaderSync
      'StWait
      'StNext
    MsgCancel :: Message MarloweHeaderSync
      'StWait
      'StIdle
    MsgIntersectFound :: BlockHeader -> Message MarloweHeaderSync
      'StIntersect
      'StIdle
    MsgIntersectNotFound :: Message MarloweHeaderSync
      'StIntersect
      'StIdle

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokWait :: ClientHasAgency 'StWait

  data ServerHasAgency st where
    TokNext :: ServerHasAgency 'StNext
    TokIntersect :: ServerHasAgency 'StIntersect

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case
  exclusionLemma_ClientAndServerHaveAgency TokWait = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case
