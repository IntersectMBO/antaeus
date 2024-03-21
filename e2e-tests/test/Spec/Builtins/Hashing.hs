{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.Builtins.Hashing where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import Hedgehog (MonadTest)
import Helpers.Common (toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusScripts.Hashing.V_1_0 qualified as PS_1_0
import PlutusScripts.Hashing.V_1_1 qualified as PS_1_1

verifyHashingFunctionsTestInfo =
  TestInfo
    { testName = "verifyHashingFunctionsTest"
    , testDescription =
        "Hashing functions can be used in a single script to mint. New hashing functions are "
          ++ "used in supporting plutus language versions"
    , test = verifyHashingFunctionsTest
    }

verifyHashingFunctionsTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
verifyHashingFunctionsTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let (tokenValues, mintWitnesses) = case era of
        C.AlonzoEra ->
          ( C.valueFromList [(PS_1_0.checkHashingAssetIdV1, 4)]
          , Map.fromList [PS_1_0.checkHashingMintWitnessV1 sbe]
          )
        C.BabbageEra ->
          ( C.valueFromList [(PS_1_0.checkHashingAssetIdV1, 4), (PS_1_0.checkHashingAssetIdV2, 2)]
          , Map.fromList [PS_1_0.checkHashingMintWitnessV1 sbe, PS_1_0.checkHashingMintWitnessV2 sbe]
          )
        C.ConwayEra ->
          ( C.valueFromList
              [ (PS_1_0.checkHashingAssetIdV1, 4)
              , (PS_1_0.checkHashingAssetIdV2, 2)
              , (PS_1_1.checkHashingAssetIdV3, 3)
              ]
          , Map.fromList
              [ PS_1_0.checkHashingMintWitnessV1 sbe
              , PS_1_0.checkHashingMintWitnessV2 sbe
              , PS_1_1.checkHashingMintWitnessV3 sbe
              ]
          )
      txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
      collateral = Tx.txInsCollateral era [txIn]
      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut]
          }

  -- Build and submit transaction
  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

  -- Query for txo and assert it contains newly minting tokens to prove successful use of SECP256k1 builtins
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue
