{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Basic.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 (Redeemer, ScriptPurpose (Minting))
import PlutusLedgerApi.V2 qualified as PlutusV2 (Map)
import PlutusScripts.Basic.Common (
  mkMintTokenNamePolicyV3,
  mkTimeRangePolicyV3,
  mkWitnessRedeemerPolicyV3,
 )
import PlutusScripts.Helpers (
  asRedeemer,
  fromPolicyId,
  mintScriptWitness,
  mintScriptWitness',
  plutusL3,
  policyIdV3,
  spendScriptWitness,
  toScriptData,
  writeSerialisedScript,
 )
import PlutusTx qualified as P
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicyV3 :: SerialisedScript
alwaysSucceedPolicyV3 =
  serialiseCompiledCode $$(P.compile [||alwaysSucceed||])
  where
    alwaysSucceed :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceed _ctx = P.check True

alwaysSucceedPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
alwaysSucceedPolicyScriptV3 = C.PlutusScriptSerialised alwaysSucceedPolicyV3

writeAlwaysSucceedPolicyScriptV3 :: IO ()
writeAlwaysSucceedPolicyScriptV3 =
  writeSerialisedScript "alwaysSucceedPolicyScriptV3" alwaysSucceedPolicyScriptV3

alwaysSucceedPolicyIdV3 :: C.PolicyId
alwaysSucceedPolicyIdV3 = policyIdV3 alwaysSucceedPolicyV3

alwaysSucceedAssetIdV3 :: C.AssetId
alwaysSucceedAssetIdV3 = C.AssetId (policyIdV3 alwaysSucceedPolicyV3) ""

alwaysSucceedPolicyTxInfoRedeemerV3 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysSucceedPolicyTxInfoRedeemerV3 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysSucceedPolicyIdV3)
    (asRedeemer $ P.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysSucceedMintWitnessV3
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV3 sbe Nothing =
  ( policyIdV3 alwaysSucceedPolicyV3
  , mintScriptWitness
      sbe
      plutusL3
      (Left alwaysSucceedPolicyScriptV3)
      (toScriptData ())
  )
alwaysSucceedMintWitnessV3 sbe (Just refTxIn) =
  ( policyIdV3 alwaysSucceedPolicyV3
  , mintScriptWitness sbe plutusL3 (Right refTxIn) (toScriptData ())
  )

alwaysSucceedMintWitnessV3'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV3' sbe exunits =
  ( policyIdV3 alwaysSucceedPolicyV3
  , mintScriptWitness'
      sbe
      plutusL3
      (Left alwaysSucceedPolicyScriptV3)
      (toScriptData ())
      exunits
  )

-- AlwaysSucceeds validator --

alwaysSucceedSpendScriptV3 :: C.PlutusScript C.PlutusScriptV3
alwaysSucceedSpendScriptV3 =
  C.PlutusScriptSerialised $
    serialiseCompiledCode $$(P.compile [||alwaysSucceed||])
  where
    alwaysSucceed :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceed _ctx = P.check True

writeAlwaysSucceedSpendScriptV3 :: IO ()
writeAlwaysSucceedSpendScriptV3 =
  writeSerialisedScript "alwaysSucceedSpendScriptV3" alwaysSucceedSpendScriptV3

alwaysSucceedSpendScriptHashV3 :: C.ScriptHash
alwaysSucceedSpendScriptHashV3 =
  C.hashScript $ C.PlutusScript C.PlutusScriptV3 alwaysSucceedSpendScriptV3

alwaysSucceedSpendWitnessV3
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn
  -> Maybe C.HashableScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV3 era mRefScript mDatum =
  C.ScriptWitness C.ScriptWitnessForSpending $
    spendScriptWitness
      era
      plutusL3
      ( maybe
          (Left alwaysSucceedSpendScriptV3)
          (\refScript -> Right refScript)
          mRefScript -- script or reference script
      )
      (C.ScriptDatumForTxIn mDatum)
      (toScriptData ()) -- redeemer

-- AlwaysFails minting policy --

alwaysFailsPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
alwaysFailsPolicyScriptV3 = C.PlutusScriptSerialised alwaysFailsPolicy

alwaysFailsPolicy :: SerialisedScript
alwaysFailsPolicy = serialiseCompiledCode $$(P.compile [||alwaysFails||])
  where
    alwaysFails :: P.BuiltinData -> P.BuiltinUnit
    alwaysFails _ctx = P.check False

writeAlwaysFailsPolicyScriptV3 :: IO ()
writeAlwaysFailsPolicyScriptV3 =
  writeSerialisedScript "alwaysFailsPolicyScriptV3" alwaysFailsPolicyScriptV3

alwaysFailsPolicyIdV3 :: C.PolicyId
alwaysFailsPolicyIdV3 = policyIdV3 alwaysFailsPolicy

alwaysFailsAssetIdV3 :: C.AssetId
alwaysFailsAssetIdV3 = C.AssetId alwaysFailsPolicyIdV3 ""

alwaysFailsPolicyTxInfoRedeemerV3 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysFailsPolicyTxInfoRedeemerV3 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysFailsPolicyIdV3)
    (asRedeemer $ P.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysFailsMintWitnessV3
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV3 sbe Nothing =
  ( policyIdV3 alwaysFailsPolicy
  , mintScriptWitness sbe plutusL3 (Left alwaysFailsPolicyScriptV3) (toScriptData ())
  )
alwaysFailsMintWitnessV3 sbe (Just refTxIn) =
  ( policyIdV3 alwaysFailsPolicy
  , mintScriptWitness sbe plutusL3 (Right refTxIn) (toScriptData ())
  )

alwaysFailsMintWitnessV3'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV3' sbe exunits =
  ( policyIdV3 alwaysFailsPolicy
  , mintScriptWitness' sbe plutusL3 (Left alwaysFailsPolicyScriptV3) (toScriptData ()) exunits
  )

-- Mint token name policy --

mintTokenNamePolicyV3 :: SerialisedScript
mintTokenNamePolicyV3 =
  serialiseCompiledCode $$(P.compile [||mkMintTokenNamePolicyV3||])

mintTokenNamePolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
mintTokenNamePolicyScriptV3 = C.PlutusScriptSerialised mintTokenNamePolicyV3

writeTokenNamePolicyScriptV3 :: IO ()
writeTokenNamePolicyScriptV3 = writeSerialisedScript "mintTokenNamePolicyScriptV3" mintTokenNamePolicyScriptV3

-- Time range policy --

timeRangePolicyV3 :: SerialisedScript
timeRangePolicyV3 =
  serialiseCompiledCode $$(P.compile [||mkTimeRangePolicyV3||])

timeRangePolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
timeRangePolicyScriptV3 = C.PlutusScriptSerialised timeRangePolicyV3

writeTimeRangePolicyScriptV3 :: IO ()
writeTimeRangePolicyScriptV3 = writeSerialisedScript "timeRangePolicyScriptV3" timeRangePolicyScriptV3

-- Witness redeemer policy --

witnessRedeemerPolicyV3 :: SerialisedScript
witnessRedeemerPolicyV3 =
  serialiseCompiledCode $$(P.compile [||mkWitnessRedeemerPolicyV3||])

witnessRedeemerPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
witnessRedeemerPolicyScriptV3 = C.PlutusScriptSerialised witnessRedeemerPolicyV3

writeWitnessRedeemerPolicyScriptV3 :: IO ()
writeWitnessRedeemerPolicyScriptV3 = writeSerialisedScript "witnessRedeemerPolicyScriptV3" witnessRedeemerPolicyScriptV3
