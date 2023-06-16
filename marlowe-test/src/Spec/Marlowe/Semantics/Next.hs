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
import Language.Marlowe.Core.V1.Semantics.Next
  ( ApplicableGeneralizedInputs(..)
  , CanReduce(CanReduce)
  , Next(applicableGeneralizedInputs, canReduce)
  , applicablesWhen
  , emptyApplicables
  , next
  )
import Language.Marlowe.Core.V1.Semantics.Next.CanChoose (compactAdjoinedBounds, overlaps)
import Language.Marlowe.Core.V1.Semantics.Next.Indexed
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Common.Isomorphism ()
import Spec.Marlowe.Semantics.Next.Common.QuickCheck (forAll')
import Spec.Marlowe.Semantics.Next.Contract.Generator
  ( anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds
  , anyCaseContractsWithChoiceOnlyNotShadowed
  , anyCaseContractsWithEmptyBoundsChoiceOnly
  , anyCaseContractsWithIdenticalEvaluatedDeposits
  , anyCaseContractsWithoutIdenticalEvaluatedDeposits
  , anyCloseOrReducedToAClose
  , anyEmptyWhenNonTimedOut
  , anyIrreducibleContract
  , anyOnlyFalsifiedNotifies
  , anyReducibleContract
  , anyWithAtLeastOneNotifyTrue
  )
import Spec.Marlowe.Semantics.Next.When.Choice (onlyIndexedChoices)
import Spec.Marlowe.Semantics.Next.When.Deposit (evaluateDeposits)
import Spec.Marlowe.Semantics.Next.When.Notify (firstNotifyTrueIndex)
import Test.QuickCheck (withMaxSuccess)
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
                evaluatedDeposits == (to . deposits. applicablesWhen environment' state $ caseContracts)
      , testProperty
          "\"Choice\" with empty bounds is not applicables"
            $ forAll' anyCaseContractsWithEmptyBoundsChoiceOnly $ \(environment', state, caseContracts) -> do
                null(choices . applicablesWhen environment' state $ caseContracts)
      , testProperty
          "\"CanChoose\" is isomorphic to \"Choice\" and its \"Case\" index preserved (when no shadowing involved)"
            $ forAll' anyCaseContractsWithChoiceOnlyNotShadowed $ \(environment', state, caseContracts) -> do
                let indexedChoices =  onlyIndexedChoices environment' state caseContracts
                indexedChoices == (to . choices . applicablesWhen environment' state $ caseContracts)
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
              "[Indexed CanChoose]'s bounds on the same choiceId don't overlap"
                $ forAll' anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds $ \(environment', state, caseContracts) -> do
                    let indexedChoices =  to . onlyIndexedChoices environment' state $ caseContracts
                        canchooseList = choices . applicablesWhen environment' state $ caseContracts
                    overlaps indexedChoices && (not. overlaps $ canchooseList)
                      || (not. overlaps $ indexedChoices) && (not. overlaps $ canchooseList)
          ,  testProperty
              "\"[Indexed CanChoose]\" and [Choice] on the same choiceId have the same merged Bounds "
                $ withMaxSuccess 50 $ forAll' anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds $ \(environment', state, caseContracts) -> do
                    let indexedChoices =  to . onlyIndexedChoices environment' state $ caseContracts
                        canchooseList = choices . applicablesWhen environment' state $ caseContracts
                    compactAdjoinedBounds indexedChoices == compactAdjoinedBounds canchooseList

          -- , testProperty
          --     "Following Overlapping Choice Bounds for a Identical Choice Ids are not applicable (shadowed)"
          --       $ forAll' anyCaseContractsWithChoiceOnly $ \(environment', state, caseContracts) -> do
          --           let indexedChoices = onlyIndexedChoices environment' state caseContracts
          --                -- CanChoose Don't overlaps for a same choiceId
          --                -- Bounds are preserves
          --                -- CanChoose with empties Bound are not applicable
          --                -- Following vs Preceding : Unit Tested?
          --                -- "\"Choices\" is a subset of \"Choice\" and its \"Case\" index preserved (when no shadowing involved)"
          --           traceShow
          --             indexedChoices
          --             (traceShow (choices . applicablesWhen environment' state $ caseContracts)
          --               (indexedChoices == ( to . choices . applicablesWhen environment' state $ caseContracts)))
          ]
      ]
  ]


