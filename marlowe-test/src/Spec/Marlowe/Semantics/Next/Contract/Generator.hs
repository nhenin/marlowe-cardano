
{-# LANGUAGE MultiParamTypeClasses #-}



module Spec.Marlowe.Semantics.Next.Contract.Generator
  ( anyCaseContractsWithIdenticalEvaluatedDeposits
  , anyCaseContractsWithoutIdenticalEvaluatedDeposits
  , anyCloseOrReducedToAClose
  , anyEmptyWhenNonTimedOut
  , anyIrreducibleContract
  , anyOnlyFalsifiedNotifies
  , anyReducibleContract
  , anyWithAtLeastOneNotifyTrue
  , anyWithValidEnvironement
  ) where


import Language.Marlowe (Case, Contract, Environment, State)
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck (Arbitrary(arbitrary), Gen, suchThat)

import Spec.Marlowe.Semantics.Next.Common.Tuple (uncurry3)
import Spec.Marlowe.Semantics.Next.Contract
  (hasValidEnvironement, isEmptyWhenNonTimedOut, isIrreducible, isNotClose, isReducible, isReducibleToClose)
import Spec.Marlowe.Semantics.Next.When.Deposit
import Spec.Marlowe.Semantics.Next.When.Notify (areOnlyFalsifiedNotifies, atLeastOneNotifyTrue)

anyReducibleContract :: Gen (Environment,State,Contract)
anyReducibleContract
  = anyContract
      `suchThat` uncurry3 isReducible
      `suchThat` uncurry3 isNotClose

anyIrreducibleContract :: Gen (Environment,State,Contract)
anyIrreducibleContract
  = anyContract
      `suchThat` uncurry3 isIrreducible
      `suchThat` uncurry3 isNotClose

anyOnlyFalsifiedNotifies :: Gen (Environment,State,Contract)
anyOnlyFalsifiedNotifies
  = anyContract `suchThat` uncurry3 areOnlyFalsifiedNotifies

anyWithAtLeastOneNotifyTrue :: Gen (Environment,State,Contract)
anyWithAtLeastOneNotifyTrue
  = anyContract `suchThat` uncurry3 atLeastOneNotifyTrue

anyCloseOrReducedToAClose :: Gen (Environment,State,Contract)
anyCloseOrReducedToAClose
  = anyContract `suchThat` uncurry3 isReducibleToClose

anyWithValidEnvironement :: Gen (Environment,State,Contract)
anyWithValidEnvironement
  = anyContract `suchThat` uncurry3 hasValidEnvironement

anyCaseContractsWithoutIdenticalEvaluatedDeposits :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithoutIdenticalEvaluatedDeposits
  = arbitrary `suchThat`  uncurry3 hasNoIdenticalEvaluatedDeposits

anyCaseContractsWithIdenticalEvaluatedDeposits :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithIdenticalEvaluatedDeposits
  = arbitrary `suchThat` uncurry3 hasIdenticalEvaluatedDeposits

anyContract :: Gen (Environment,State,Contract)
anyContract
  = arbitrary

anyEmptyWhenNonTimedOut :: Gen (Environment,State,Contract)
anyEmptyWhenNonTimedOut
  = arbitrary `suchThat` uncurry3 isEmptyWhenNonTimedOut
