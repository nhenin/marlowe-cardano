#!/usr/bin/env runhaskell

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.ByteString.Lazy.Char8 as C
import Shh
import System.Environment (getEnv)
import Data.List 

load SearchPath ["grep","pwd", "cat", "marlowe-cli", "marlowe-runtime-cli", "cardano-cli" ,"echo"]

data Sponsor = Sponsor {s_address :: String, s_collateral :: String, s_privateKeyFilePath :: String}
data Oracle = Oracle {o_address :: String}
data RaffleConfiguration = RaffleConfiguration {contract :: FilePath, state :: FilePath, tmpTxToSign :: FilePath, tmpTxToSubmit :: FilePath, chunkSize :: Integer}
data RuntimeURI = RuntimeURI {host :: String, port :: Integer}
data Deadlines = Deadlines {deposit :: String, selectWinner :: String, payout :: String}
type PolicyId = String
type TokenName = String
type ContractId = String

main :: IO ()
main = do
  s_address <- C.unpack <$> (cat "./addresses/sponsor/address.txt" |> captureTrim)
  s_collateral <- C.unpack <$> (cat "./addresses/sponsor/collateral.txt" |> captureTrim)
  nodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH" 
  rootPath <- C.unpack <$> (pwd |> captureTrim)
  echo s_address
  echo s_collateral
  let runtimeURI = RuntimeURI "preprod.marlowe.run" 3700 -- "0.0.0.0" 56063
      raffleConfiguration =
        RaffleConfiguration
          { contract = rootPath ++ "/raffle.json"
          , state = rootPath ++ "/raffle-state.json"
          , tmpTxToSign = rootPath ++ "/unsigned-tx.txt"
          , tmpTxToSubmit = rootPath ++ "/signed-tx.txt"
          , chunkSize = 2
          }

      sponsor = 
        Sponsor{ s_address = s_address
               , s_collateral = s_collateral
               , s_privateKeyFilePath = rootPath ++ "/addresses/sponsor/extended-private-key.json"}
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

  prizes <- mintPrizeTokens runtimeURI raffleConfiguration sponsor nodeSocketPath ["1stPrize","2ndPrize" ,"3rdPrize"]

  executeRaffle
    runtimeURI
    raffleConfiguration
    sponsor
    oracle
    deadlines
    parties
    prizes

mintPrizeTokens :: RuntimeURI -> RaffleConfiguration -> Sponsor -> TokenName -> [String] ->IO [(PolicyId,TokenName)]
mintPrizeTokens runtime raffleConfiguration sponsor nodeSocketPath tokenNames = do
  policyID <- (C.unpack) <$> ( marlowe_cli 
      "util" 
      "mint"
      "--testnet-magic" 1 --preprod
      "--socket-path" nodeSocketPath
      "--issuer" ((s_address sponsor) ++ ":" ++ (s_privateKeyFilePath sponsor))
      "--count" 1
      "--out-file" (tmpTxToSign raffleConfiguration)
      (asArg $ (\tokenName -> tokenName ++ ":" ++ (s_address sponsor)) <$> tokenNames) |> captureTrim)
  
  echo $ " >> tx unsigned"
  cardano_cli 
    "transaction" 
    "sign"
    "--signing-key-file" (s_privateKeyFilePath sponsor)
    "--tx-body-file" (tmpTxToSign raffleConfiguration)
    "--out-file" (tmpTxToSubmit raffleConfiguration)
  echo $ " >> tx signed"
  marlowe_runtime_cli
    "--marlowe-runtime-host" (host runtime)
    "--marlowe-runtime-port" (port runtime)
    "submit" 
    (tmpTxToSubmit raffleConfiguration)
  let mintedTokens = (policyID,) <$> tokenNames
  echo $ ">> minted NFT Tokens : " ++  show mintedTokens
  return mintedTokens
  

executeRaffle :: RuntimeURI -> RaffleConfiguration -> Sponsor -> Oracle -> Deadlines -> [String] -> [(PolicyId,TokenName)] -> IO ()
executeRaffle runtime raffleConfiguration sponsor oracle deadlines parties prizes = do
  echo "#########################"
  echo "Raffle Contract Generation"
  echo "-------------------------"
  generateContract
  contractHash <- loadContractToStore
  contractId <- initialize contractHash
  sequence $ (\(a,b) -> depositNFT contractId a b) <$> prizes
  echo "#########################"
  where
    generateContract :: IO ()
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
        (asArg $ (\(a,b) -> ["--prizes", a ++ "." ++ b]) <$> prizes)
        "--out-contract-file" (contract raffleConfiguration)
        "--out-state-file" (state raffleConfiguration) 
      echo $ " >> Raffle Contract saved in : " ++ (contract raffleConfiguration)

    loadContractToStore = do
      contractHash <-
        C.unpack
          <$> ( marlowe_runtime_cli
                  "--marlowe-runtime-host" (host runtime)
                  "--marlowe-runtime-port" (port runtime)
                  "load"
                  (contract raffleConfiguration)
                  |> captureTrim
              )
      echo $ " >> Contract stored with hash :" ++ contractHash
      return contractHash

    initialize :: String -> IO ContractId
    initialize contractHash = do
      contractId <- C.unpack  <$> (marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "create"
        "--min-utxo" 2_000_000
        "--change-address"  (s_address sponsor)
        "--collateral-utxo" (s_collateral sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract-hash"   contractHash |> captureTrim)
      echo $ " >> tx unsigned"
      echo $ contractId
      cardano_cli 
        "transaction" 
        "sign"
        "--signing-key-file" (s_privateKeyFilePath sponsor)
        "--tx-body-file" (tmpTxToSign raffleConfiguration)
        "--out-file" (tmpTxToSubmit raffleConfiguration)
      echo $ " >> tx signed"
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "submit" 
        (tmpTxToSubmit raffleConfiguration)
      echo $ " >> Contract initialzed (tx appended)"
      return contractId

    depositNFT :: ContractId -> PolicyId -> TokenName -> IO()
    depositNFT contractId policyId tokenName = do
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "deposit"
        "--change-address"  (s_address sponsor)
        "--collateral-utxo" (s_collateral sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract" contractId
        "--to-party" (s_address sponsor)
        "--from-party" (s_address sponsor) 
        "--currency" policyId
        "--token-name" tokenName
        "--quantity" 1
      echo $ " >> tx unsigned"
      cardano_cli 
        "transaction" 
        "sign"
        "--signing-key-file" (s_privateKeyFilePath sponsor)
        "--tx-body-file" (tmpTxToSign raffleConfiguration)
        "--out-file" (tmpTxToSubmit raffleConfiguration)
      echo $ " >> tx signed"
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "submit" 
        (tmpTxToSubmit raffleConfiguration)
      echo $ " >> Token Deposited " ++ tokenName       

-- Usage: marlowe-runtime-cli deposit --change-address ADDRESS 
--                                    [-a|--address ADDRESS] 
--                                    [--collateral-utxo UTXO]
--                                    --manual-sign FILE_PATH 
--                                    [-m|--metadata-file FILE_PATH] 
--                                    [--tags-file FILE_PATH]
--                                    (-c|--contract CONTRACT_ID)
--                                    --to-party ROLE_NAME|ADDRESS
--                                    --from-party ROLE_NAME|ADDRESS 
--                                    ((-c|--currency MINTING_POLICY_ID)
--                                      (-n|--token-name TOKEN_NAME)
--                                      (-q|--quantity INTEGER) |
--                                      (-l|--lovelace INTEGER)) 
--                                    [--continuation-file FILE_PATH] 
--                                    [-l|--validity-lower-bound TIMESTAMP] 
--                                    [-u|--validity-upper-bound TIMESTAMP]

--   Deposit funds into a contract.


    notify contractId =
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (port runtime)
        "notify"
        "--change-address"  (s_address sponsor)
        "--collateral-utxo" (s_collateral sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract" contractId
