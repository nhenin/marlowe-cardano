
module Spec.Marlowe.Semantics.Entropy (
  tests
) where


import Language.Marlowe.Core.V1.Semantics.Types (Accounts, ChoiceId, ChosenNum, Party, Token, ValueId)
import Plutus.V1.Ledger.Api (CurrencySymbol, PubKeyHash, TokenName)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryChoiceName)
import Spec.Marlowe.Semantics.Orphans ()
import Spec.Marlowe.Semantics.Util (checkEntropy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen)

import qualified PlutusTx.AssocMap as AM


tests :: TestTree
tests =
  testGroup "Arbitrary"
    [
      testGroup "Entropy"
        [
          testCase "PubKeyHash"     $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen PubKeyHash                               )
        , testCase "CurrencySymbol" $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen CurrencySymbol                           )
        , testCase "TokenName"      $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen TokenName                                )
        , testCase "Token"          $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen Token                                    )
        , testCase "Party"          $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen Party                                    )
        , testCase "ChoiceName"     $ checkEntropy 1000 (logBase 2 5, logBase 2 100)  arbitraryChoiceName
        , testCase "ChoiceId"       $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen ChoiceId                                 )
        , testCase "ValueId"        $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen ValueId                                  )
        , testCase "accounts"       $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> (arbitrary :: Gen Accounts                   ))
        , testCase "choices"        $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> (arbitrary :: Gen (AM.Map ChoiceId ChosenNum)))
        , testCase "boundValues"    $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> (arbitrary :: Gen (AM.Map ValueId Integer   )))
        ]
    ]
