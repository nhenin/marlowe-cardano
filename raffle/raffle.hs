#!/usr/bin/env runhaskell

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Lazy.Char8 as C
import Shh

load SearchPath ["pwd", "cat", "marlowe-cli", "marlowe-runtime-cli", "echo"]

data Sponsor = Sponsor {s_address :: String, s_collateral :: String}
data Oracle = Oracle {o_address :: String}
data RaffleConfiguration = RaffleConfiguration {contract :: FilePath, state :: FilePath, tmpTxToSign :: FilePath, chunkSize :: Integer}
data RuntimeURI = RuntimeURI {host :: String, port :: Integer}
data Deadlines = Deadlines {deposit :: String, selectWinner :: String, payout :: String}

main :: IO ()
main = do
  s_address <- C.unpack <$> (cat "./addresses/sponsor/address.txt" |> capture)
  s_collateral <- C.unpack <$> (cat "./addresses/sponsor/collateral.txt" |> capture)
  rootPath <- C.unpack <$> (pwd |> captureTrim)
  echo s_address
  echo s_collateral
  let runtimeURI = RuntimeURI "preprod.marlowe.run" 3700 -- "0.0.0.0" 56063
      raffleConfiguration =
        RaffleConfiguration
          { contract = rootPath ++ "/raffle.json"
          , state = rootPath ++ "/raffle-state.json"
          , tmpTxToSign = rootPath ++ "/unsigned-tx.txt"
          , chunkSize = 2
          }

      sponsor = Sponsor{s_address = s_address, s_collateral = s_collateral}
      oracle =
        Oracle
          { o_address =
              "addr_test1qzy4aluz9zcmxuhuqgjhl457ee0ma0hcq38w62ckaagjwgvnjlzdfawd9kvva673fht7737e3r5j322v7090uqhpn0wq7a2gly"
          }
      deadlines = Deadlines "10d" "10d" "10d"

      parties =
        [ "addr_test1qpj0y6wace4m6qfrs3gnw8jefql4ajy9y8fuu7hdpgdqv9vtgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42qnm58x5"
        , "addr_test1qpx28jszrx6n90shly5qvdqzaxmrhrkr7nleq3u6ayfz375tgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42q4ckex2"
        , "addr_test1qpx28jszrx6n90shly5qvdqzaxmrhrkr7nleq3u6ayfz375tgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42q4ckex2"
        , "addr_test1qpx28jszrx6n90shly5qvdqzaxmrhrkr7nleq3u6ayfz375tgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42q4ckex2"
        ]

      prizes =
        [ "1f7a58a1aa1e6b047a42109ade331ce26c9c2cce027d043ff264fb1f.1stPrize"
        , "1f7a58a1aa1e6b047a42109ade331ce26c9c2cce027d043ff264fb2f.2ndPrize"
        , "1f7a58a1aa1e6b047a42109ade331ce26c9c2cce027d043ff264fb3f.3rdPrize"
        ]

  executeRaffle
    runtimeURI
    raffleConfiguration
    sponsor
    oracle
    deadlines
    parties
    prizes

-- mintPrizeTokens :: [String] ->IO ()
-- mintPrizeTokens tokenNames = do

executeRaffle :: RuntimeURI -> RaffleConfiguration -> Sponsor -> Oracle -> Deadlines -> [String] -> [String] -> IO ()
executeRaffle runtime raffleConfiguration sponsor oracle deadlines parties prizes = do
  echo "#########################"
  echo "Raffle Contract Generation"
  echo "-------------------------"
  generateContract
  raffleLoadedHash <- loadContractToStore
  initialize raffleLoadedHash

  echo "#########################"
  where
    generateContract = do
      marlowe_cli
        "template"
        "raffle"
        "--minimum-ada" 2_000_000
        "--sponsor" (s_address sponsor)
        "--oracle"  (o_address oracle)
        "--chunk-size" (chunkSize raffleConfiguration)
        (asArg $ (\party -> ["--parties", party]) <$> parties)
        "--deposit-deadline" (deposit deadlines)
        "--select-deadline"  (selectWinner deadlines)
        "--payout-deadline"  (payout deadlines)
        (asArg $ (\prize -> ["--prizes", prize]) <$> prizes)
        "--out-contract-file" (contract raffleConfiguration)
        "--out-state-file" (state raffleConfiguration)
      echo $ " >> Raffle Contract saved in : " ++ (contract raffleConfiguration)

    loadContractToStore = do
      raffleLoadedHash <-
        C.unpack
          <$> ( marlowe_runtime_cli
                  "--marlowe-runtime-host" (host runtime)
                  "--marlowe-runtime-port" (port runtime)
                  "load"
                  (contract raffleConfiguration)
                  |> captureTrim
              )
      echo $ " >> Contract stored with hash :" ++ raffleLoadedHash
      return raffleLoadedHash

    initialize :: String -> IO ()
    initialize raffleLoadedHash = do
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "create"
        "--min-utxo" 2_000_000
        "--change-address"  (s_address sponsor)
        "--collateral-utxo" (s_collateral sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract-hash"   raffleLoadedHash

    applyNotify contractId =
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "notify"
        "--change-address"  (s_address sponsor)
        "--collateral-utxo" (s_collateral sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract" contractId
