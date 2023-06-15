{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marlowe.Semantics.Next
  ( -- * Testing
    tests
  ) where

import Data.Coerce (coerce)
import Data.List (nubBy)
import Data.Maybe (fromJust)
import Data.Types.Isomorphic (Injective(to))
import Debug.Trace
import Language.Marlowe.Core.V1.Semantics.Next
  ( ApplicableGeneralizedInputs(..)
  , CanReduce(CanReduce)
  , Next(applicableGeneralizedInputs, canReduce)
  , applicablesWhen
  , emptyApplicables
  , next
  )
import Language.Marlowe.Core.V1.Semantics.Next.Indexed
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Common.Isomorphism ()
import Spec.Marlowe.Semantics.Next.Common.QuickCheck (forAll')
import Spec.Marlowe.Semantics.Next.Contract.Generator
  ( anyCaseContractsWithIdenticalEvaluatedDeposits
  , anyCaseContractsWithoutIdenticalEvaluatedDeposits
  , anyCloseOrReducedToAClose
  , anyEmptyWhenNonTimedOut
  , anyIrreducibleContract
  , anyOnlyFalsifiedNotifies
  , anyReducibleContract
  , anyWithAtLeastOneNotifyTrue
  , anyWithValidEnvironement
  )
import Spec.Marlowe.Semantics.Next.When.Choice (onlyIndexedChoices)
import Spec.Marlowe.Semantics.Next.When.Deposit (evaluateDeposits)
import Spec.Marlowe.Semantics.Next.When.Notify (firstNotifyTrueIndex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Next"
  [ testGroup "Reducibility"
      [ testProperty
          "Can Reduce when contract is reducible"
            $ forAll' anyReducibleContract $ \(environment', state, contract) ->
                Right (coerce True) == (canReduce <$> next environment' state contract)
      , testProperty
          "Can't Reduce when the contract provided is irreducible"
            $ forAll' anyIrreducibleContract $ \(environment', state, contract) ->
                Right (coerce False) == (canReduce <$> next environment' state contract)
      , testProperty
          "Can Reduce a \"Close\""
            $ forAll' anyCloseOrReducedToAClose $ \(environment', state, contract) ->
                Right (coerce True) == (canReduce <$> next environment' state contract)
      ]
  , testGroup "Applicability"
      [ testProperty
          "Notify is not applicable when evaluated falsified"
            $ forAll' anyOnlyFalsifiedNotifies $ \(environment', state, contract) ->
                Right Nothing == (canNotifyMaybe . applicableGeneralizedInputs <$> next environment' state contract)
      , testProperty
          "Non timed out empty \"When\" is not applicable"
            $ forAll' anyEmptyWhenNonTimedOut $ \(environment', state, contract) ->
                Right emptyApplicables == ( applicableGeneralizedInputs <$> next environment' state contract)
      , testProperty
          "\"Close\" is not applicable"
            $ forAll' anyCloseOrReducedToAClose $ \(environment', state, contract) ->
                Right emptyApplicables == (applicableGeneralizedInputs <$> next environment' state contract)
      , testProperty
          "\"CanDeposit\" is a \"Deposit\" with its quantity evaluated and its \"Case\" index preserved (when no shadowing involved)"
            $ forAll' anyCaseContractsWithoutIdenticalEvaluatedDeposits $ \(environment', state, caseContracts) -> do
                let evaluatedDeposits = evaluateDeposits environment' state caseContracts
                    canDeposits = deposits. applicablesWhen environment' state $ caseContracts
                evaluatedDeposits == to canDeposits
      , testProperty
          "\"CanChoose\" is a subset of \"Choice\" and its \"Case\" index preserved (when no shadowing involved)"
            $ forAll' anyWithValidEnvironement $ \(environment', state, contract) -> do
                let indexedChoices = onlyIndexedChoices environment' state contract
                Right indexedChoices == ( to . choices . applicableGeneralizedInputs <$> next environment' state contract)

      , testGroup "Input Shadowing"
          [ testProperty
              "Following Notifies evaluated to True are not applicable (shadowed)"
                $ forAll' anyWithAtLeastOneNotifyTrue $ \(environment', state, contract) -> do
                    let expectedCaseIndex = fromJust . firstNotifyTrueIndex environment' state $ contract
                    (Right . Just $ expectedCaseIndex ) == ( (getCaseIndex <$>). canNotifyMaybe . applicableGeneralizedInputs <$> next environment' state contract)
          , testProperty
              "Following Identical Evaluated Deposits are not applicable (shadowed)"
                $ forAll' anyCaseContractsWithIdenticalEvaluatedDeposits $ \(environment', state, caseContracts) -> do
                    let evaluatedDeposits = evaluateDeposits environment' state caseContracts
                        canDeposits = to. deposits. applicablesWhen environment' state $ caseContracts
                    canDeposits == nubBy sameIndexedValue evaluatedDeposits
          , testProperty
              "Following Overlapping Choice Bounds for an Identical Choice Id are not applicable (shadowed)"
                $ forAll' anyWithValidEnvironement $ \(environment', state, contract) -> do
                    let indexedChoices = onlyIndexedChoices environment' state contract
                         -- CanChoose Don't overlaps for a same choiceId
                         -- Bounds are preserves
                         -- CanChoose with empties Bound are not applicable
                         -- Following vs Preceding : Unit Tested?
                    traceShow
                      indexedChoices
                      (traceShow (choices . applicableGeneralizedInputs <$> next environment' state contract)
                        (Right indexedChoices == ( to . choices . applicableGeneralizedInputs <$> next environment' state contract)))
          ]
      ]
  ]


