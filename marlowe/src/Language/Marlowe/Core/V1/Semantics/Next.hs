-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for Marlowe semantics
--
-----------------------------------------------------------------------------



{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Core.V1.Semantics.Next
  ( AmbiguousIntervalProvided(..)
  , ApplicableGeneralizedInputs(..)
  , CanChoose(..)
  , CanDeposit(..)
  , CanNotify(..)
  , CanReduce(..)
  , Next(..)
  , applicables
  , applicablesWhen
  , emptyApplicables
  , next
  , sameIndexedValue
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.Aeson (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.Aeson.Types ()
import Data.Bifunctor (Bifunctor(first))
import Data.Coerce (coerce)
import Data.List.Index (indexed)
import Data.Monoid as Haskell (First(First))

import Data.List (nubBy)
import Deriving.Aeson (Generic)
import Language.Marlowe.Core.V1.Semantics
  ( ReduceResult(ContractQuiescent, RRAmbiguousTimeIntervalError)
  , evalObservation
  , evalValue
  , reduceContractUntilQuiescent
  )
import Language.Marlowe.Core.V1.Semantics.Types
  (AccountId, Action(Choice, Deposit, Notify), Case(..), Contract(Close, When), Environment, Party, State, Token)
import Language.Marlowe.Pretty (Pretty(..))


import Data.Maybe
import Language.Marlowe.Core.V1.Semantics.Next.CanChoose
import Language.Marlowe.Core.V1.Semantics.Next.Indexed
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation
import Prelude



data Next
  = Next
      { canReduce :: CanReduce
      , applicableGeneralizedInputs :: ApplicableGeneralizedInputs}
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)

data AmbiguousIntervalProvided = AmbiguousIntervalProvided
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)



data ApplicableGeneralizedInputs
    = ApplicableGeneralizedInputs
     { canNotifyMaybe :: Maybe (Indexed CanNotify)
     , deposits       :: [Indexed CanDeposit]
     , choices        :: [Indexed CanChoose]}
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)


newtype CanNotify = CanNotify IsMerkleizedContinuation
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)

data CanDeposit = CanDeposit Party AccountId Token Integer IsMerkleizedContinuation
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)

newtype CanReduce = CanReduce { unCanReduce :: Bool}
    deriving stock (Show,Eq,Ord,Generic)
    deriving newtype (FromJSON,ToJSON,Pretty)

-- | Describe for a given contract which inputs can be applied it can be reduced or not
next :: Environment ->  State  ->  Contract -> Either AmbiguousIntervalProvided Next
next environment state contract
  = do
    (canReduce,reducedState,reducedContract) <- reduceContract environment state contract
    Right
      $ Next
        canReduce
        (applicables environment reducedState reducedContract)


emptyApplicables :: ApplicableGeneralizedInputs
emptyApplicables = ApplicableGeneralizedInputs Nothing mempty mempty

-- | Describe for a given contract which inputs can be applied
applicables :: Environment  -> State  ->  Contract -> ApplicableGeneralizedInputs
applicables  _  _ Close = emptyApplicables
applicables environment state (When xs _ _) = applicablesWhen environment state xs
applicables _  _  _  = emptyApplicables

applicablesWhen :: Environment -> State -> [Case Contract] -> ApplicableGeneralizedInputs
applicablesWhen environment state
  = foldl mergeApplicables emptyApplicables
  . (uncurry (toApplicable environment state)  <$>)
  . caseIndexed

mergeApplicables
  :: ApplicableGeneralizedInputs
  -> ( Maybe (Indexed CanNotify)
     , Maybe (Indexed CanDeposit)
     , Maybe (Indexed CanChoose))
  -> ApplicableGeneralizedInputs
mergeApplicables b ( a@Just{}, _, _)
  = ApplicableGeneralizedInputs
      (coerce $ (First . canNotifyMaybe $ b) <> First a  )
      (deposits b)
      (choices b)
mergeApplicables b ( _, Just a, _)
  = ApplicableGeneralizedInputs
      (canNotifyMaybe b)
      (nubBy sameIndexedValue $ deposits b ++ [a]  )
      (choices b)
mergeApplicables b ( _, _, Just a)
  = ApplicableGeneralizedInputs
      (canNotifyMaybe b)
      (deposits b)
      ( choices b ++ (maybeToList $ a `difference` choices b))
mergeApplicables b _  = b

toApplicable
  :: Environment
  -> State
  -> CaseIndex
  -> Case Contract
  ->  ( Maybe (Indexed CanNotify)
      , Maybe (Indexed CanDeposit)
      , Maybe (Indexed CanChoose))
toApplicable environment state caseIndex
  = \case
      (merkleizedContinuation,Deposit accountId party token value)
        -> ( Nothing
           , Just (Indexed caseIndex $ CanDeposit accountId party token (evalValue environment state value ) merkleizedContinuation)
           , Nothing )
      (merkleizedContinuation,Choice choiceId bounds)
        -> ( Nothing
           , Nothing
           , Just $ Indexed  caseIndex $ CanChoose choiceId bounds merkleizedContinuation)
      (merkleizedContinuation,Notify observation) | evalObservation environment state observation
        -> ( Just . Indexed  caseIndex $ CanNotify merkleizedContinuation
           , Nothing
           , Nothing)
      (_,Notify _) -> (Nothing,Nothing, Nothing)
  . \case
      (Case action _)           -> (IsMerkleizedContinuation False,action)
      (MerkleizedCase action _) -> (IsMerkleizedContinuation True,action)


caseIndexed :: [a] -> [(CaseIndex,a)]
caseIndexed xs = first (CaseIndex . fromIntegral) <$> indexed  xs

reduceContract :: Environment -> State -> Contract -> Either AmbiguousIntervalProvided (CanReduce,State,Contract)
reduceContract environment state
  = (\case
      ContractQuiescent _ _ _ newState Close -> Right (CanReduce True ,newState,Close)  -- Todo : Add an extra notion of Terminate (N.H)
      ContractQuiescent isReduced _ _ newState newContract -> Right (CanReduce isReduced ,newState,newContract)
      RRAmbiguousTimeIntervalError -> Left AmbiguousIntervalProvided)
    . reduceContractUntilQuiescent environment state


instance FromJSON Next where
  parseJSON (Object v)
    = Next
        <$> v .: "can_reduce"
        <*> v .: "applicable_generalized_inputs"
  parseJSON _ = fail "Next must be an object with 2 fields \"can_reduce\" and \"applicable_generalized_inputs\""

instance ToJSON Next where
  toJSON Next {..}
    = object [ "can_reduce" .= canReduce
             , "applicable_generalized_inputs" .= applicableGeneralizedInputs]


instance FromJSON ApplicableGeneralizedInputs where
  parseJSON (Object v)
    =   ApplicableGeneralizedInputs
          <$> v .: "notify"
          <*> v .: "deposits"
          <*> v .: "choices"
    <|> ApplicableGeneralizedInputs Nothing <$> (v .: "deposits")
          <*> v .: "choices"
  parseJSON _ = fail "NextGeneralizedInput must be either an object "

instance ToJSON ApplicableGeneralizedInputs where
  toJSON ApplicableGeneralizedInputs {canNotifyMaybe = Nothing,..} = object
      [ "deposits" .= deposits
      , "choices"  .= choices
      ]
  toJSON ApplicableGeneralizedInputs {canNotifyMaybe = Just notify,..} = object
      [ "notify"   .= notify
      , "deposits" .= deposits
      , "choices"  .= choices
      ]


instance FromJSON (Indexed CanNotify) where
  parseJSON (Object v) =  Indexed <$> (CaseIndex <$> v .: "case_index")
                                  <*> (CanNotify <$> v .: "is_merkleized_continuation")
  parseJSON _ = fail "CanDeposit must be an object with 1 field \"is_merkleized_continuation\""

instance ToJSON (Indexed CanNotify) where
  toJSON (Indexed caseindex (CanNotify isMerkleizedContinuation)) = object
      [ "case_index" .= caseindex
      , "is_merkleized_continuation" .= isMerkleizedContinuation]

instance FromJSON (Indexed CanDeposit) where
  parseJSON (Object v)
    =  Indexed
         <$>  (CaseIndex <$> v .: "case_index")
         <*>  (CanDeposit
                <$> v .: "party"
                <*> v .: "into_account"
                <*> v .: "of_token"
                <*> v .: "can_deposit"
                <*> v .: "is_merkleized_continuation")
  parseJSON _ = fail "CanDeposit must be either an object"

instance ToJSON (Indexed CanDeposit) where
  toJSON (Indexed caseIndex (CanDeposit party accountId token quantity isMerkleizedContinuation)) = object
      [ "party" .= party
      , "can_deposit" .= quantity
      , "of_token" .= token
      , "into_account" .= accountId
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]



