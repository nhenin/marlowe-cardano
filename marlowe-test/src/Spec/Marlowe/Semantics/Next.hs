{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marlowe.Semantics.Next
  ( -- * Testing
    tests
  ) where


import Data.Coerce (coerce)
import Data.Data (Proxy(..))
import Data.Maybe (fromJust)
import Data.Types.Isomorphic (Injective(to))
import Language.Marlowe.Core.V1.Semantics.Next
  ( ApplicableGeneralizedInputs(..)
  , CanReduce(CanReduce)
  , IndexedCanChooseList
  , IndexedCanDeposits
  , Next(applicableGeneralizedInputs, canReduce)
  , emptyApplicables
  , getCaseIndex
  , next
  )
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Common.Isomorphism ()
import Spec.Marlowe.Semantics.Next.Common.QuickCheck (forAll')
import Spec.Marlowe.Semantics.Next.Contract.Generator
  ( anyCloseOrReducedToAClose
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
import Test.Tasty.QuickCheck.Laws (testSemigroupLaws)


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
          "\"CanDeposit\" is a \"Deposit\" with its quantity evaluated and \"Case\" indexes are preserved"
            $ forAll' anyWithValidEnvironement $ \(environment', state, contract) -> do
                let evaluatedDeposits = evaluateDeposits environment' state contract
                Right evaluatedDeposits == ( to . deposits . applicableGeneralizedInputs <$> next environment' state contract)
      , testProperty
          "\"CanChoose\" is isomorphic to \"Choice\" and \"Case\" indexes are preserved"
            $ forAll' anyWithValidEnvironement $ \(environment', state, contract) -> do
                let indexedChoices = onlyIndexedChoices environment' state contract
                Right indexedChoices == ( to . choices . applicableGeneralizedInputs <$> next environment' state contract)
      , testGroup "Input Shadowing"
          [ testProperty
              "Only the first Notify evaluated to True is applicable"
                $ forAll' anyWithAtLeastOneNotifyTrue $ \(environment', state, contract) -> do
                    let expectedCaseIndex = fromJust . firstNotifyTrueIndex environment' state $ contract
                    (Right . Just $ expectedCaseIndex ) == ( (getCaseIndex <$>). canNotifyMaybe . applicableGeneralizedInputs <$> next environment' state contract)
          ]
      , testGroup "Laws"
          [ testSemigroupLaws (Proxy :: Proxy ApplicableGeneralizedInputs)
          , testSemigroupLaws (Proxy :: Proxy IndexedCanDeposits)
          , testSemigroupLaws (Proxy :: Proxy IndexedCanChooseList)
          ]
      ]
  ]

-- Following Identical Evaluated Deposits are not applicable
-- Following Choice having the same ChoiceId propose Bounds wi(Intersection of all the Bounds for a set of Choice having the same Choice Id is empty
