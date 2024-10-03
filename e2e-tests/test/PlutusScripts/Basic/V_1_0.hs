{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

{-# HLINT ignore "Replace case with maybe" #-}

module PlutusScripts.Basic.V_1_0 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 (Redeemer, ScriptPurpose (Minting))
import PlutusLedgerApi.V2 qualified as V2
import PlutusScripts.Helpers (
  asRedeemer,
  fromPolicyId,
  mintScriptWitness,
  mintScriptWitness',
  plutusL1,
  plutusL2,
  policyIdV1,
  policyIdV2,
  spendScriptWitness,
  toScriptData,
  unPlutusScriptV1,
  unPlutusScriptV2,
 )
import PlutusTx qualified as P
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins qualified as P

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicy :: SerialisedScript
alwaysSucceedPolicy =
  serialiseCompiledCode $$(P.compile [||alwaysSucceed||])
  where
    alwaysSucceed :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceed _redeemer _ctx = ()

alwaysSucceedPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedPolicyScriptV1 = C.PlutusScriptSerialised alwaysSucceedPolicy

alwaysSucceedPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedPolicyScriptV2 = C.PlutusScriptSerialised alwaysSucceedPolicy

alwaysSucceedPolicyIdV2 :: C.PolicyId
alwaysSucceedPolicyIdV2 = policyIdV2 alwaysSucceedPolicy

alwaysSucceedPolicyScriptHashV1 :: C.ScriptHash
alwaysSucceedPolicyScriptHashV1 =
  C.hashScript $ unPlutusScriptV1 alwaysSucceedPolicyScriptV1

alwaysSucceedPolicyScriptHashV2 :: C.ScriptHash
alwaysSucceedPolicyScriptHashV2 =
  C.hashScript $ unPlutusScriptV2 alwaysSucceedPolicyScriptV2

alwaysSucceedAssetIdV1 :: C.AssetId
alwaysSucceedAssetIdV1 = C.AssetId (policyIdV1 alwaysSucceedPolicy) ""

alwaysSucceedAssetIdV2 :: C.AssetId
alwaysSucceedAssetIdV2 = C.AssetId alwaysSucceedPolicyIdV2 ""

alwaysSucceedPolicyTxInfoRedeemerV2 :: V2.Map ScriptPurpose Redeemer
alwaysSucceedPolicyTxInfoRedeemerV2 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysSucceedPolicyIdV2)
    (asRedeemer $ P.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysSucceedMintWitnessV1
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV1 era Nothing =
  ( policyIdV1 alwaysSucceedPolicy
  , mintScriptWitness
      era
      plutusL1
      (Left alwaysSucceedPolicyScriptV1)
      (toScriptData ())
  )
alwaysSucceedMintWitnessV1 era (Just refTxIn) =
  ( policyIdV1 alwaysSucceedPolicy
  , mintScriptWitness era plutusL1 (Right refTxIn) (toScriptData ())
  )

alwaysSucceedMintWitnessV1'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV1' era exunits =
  ( policyIdV1 alwaysSucceedPolicy
  , mintScriptWitness'
      era
      plutusL1
      (Left alwaysSucceedPolicyScriptV1)
      (toScriptData ())
      exunits
  )

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysSucceedMintWitnessV2
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2 era Nothing =
  ( policyIdV2 alwaysSucceedPolicy
  , mintScriptWitness
      era
      plutusL2
      (Left alwaysSucceedPolicyScriptV2)
      (toScriptData ())
  )
alwaysSucceedMintWitnessV2 era (Just refTxIn) =
  ( policyIdV2 alwaysSucceedPolicy
  , mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ())
  )

alwaysSucceedMintWitnessV2'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2' era exunits =
  ( policyIdV2 alwaysSucceedPolicy
  , mintScriptWitness'
      era
      plutusL2
      (Left alwaysSucceedPolicyScriptV2)
      (toScriptData ())
      exunits
  )

-- AlwaysSucceeds validator --

alwaysSucceedSpendV12 :: SerialisedScript
alwaysSucceedSpendV12 =
  serialiseCompiledCode $$(P.compile [||alwaysSucceed||])
  where
    alwaysSucceed :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceed _datum _redeemer _ctx = ()

alwaysSucceedSpendScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedSpendScriptV1 =
  C.PlutusScriptSerialised alwaysSucceedSpendV12

alwaysSucceedSpendScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedSpendScriptV2 =
  C.PlutusScriptSerialised alwaysSucceedSpendV12

alwaysSucceedSpendScriptHashV1 :: C.ScriptHash
alwaysSucceedSpendScriptHashV1 =
  C.hashScript $ unPlutusScriptV1 alwaysSucceedSpendScriptV1

alwaysSucceedSpendScriptHashV2 :: C.ScriptHash
alwaysSucceedSpendScriptHashV2 =
  C.hashScript $ unPlutusScriptV2 alwaysSucceedSpendScriptV2

alwaysSucceedSpendWitnessV1
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn
  -> Maybe C.HashableScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV1 era mRefScript mDatum =
  C.ScriptWitness C.ScriptWitnessForSpending $
    spendScriptWitness
      era
      plutusL1
      ( maybe
          (Left alwaysSucceedSpendScriptV1)
          (\refScript -> Right refScript)
          mRefScript -- script or reference script
      )
      (C.ScriptDatumForTxIn mDatum)
      (toScriptData ()) -- redeemer

alwaysSucceedSpendWitnessV2
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn
  -> Maybe C.HashableScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV2 era mRefScript mDatum =
  C.ScriptWitness C.ScriptWitnessForSpending $
    spendScriptWitness
      era
      plutusL2
      ( maybe
          (Left alwaysSucceedSpendScriptV2)
          (\refScript -> Right refScript)
          mRefScript -- script or reference script
      )
      (C.ScriptDatumForTxIn mDatum)
      (toScriptData ()) -- redeemer

-- AlwaysFails minting policy --

alwaysFailsPolicyV12 :: SerialisedScript
alwaysFailsPolicyV12 = serialiseCompiledCode $$(P.compile [||alwaysFails||])
  where
    alwaysFails :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFails _redeemer _ctx = P.error ()

alwaysFailsPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysFailsPolicyScriptV1 = C.PlutusScriptSerialised alwaysFailsPolicyV12

alwaysFailsPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysFailsPolicyScriptV2 = C.PlutusScriptSerialised alwaysFailsPolicyV12

alwaysFailsPolicyIdV2 :: C.PolicyId
alwaysFailsPolicyIdV2 = policyIdV2 alwaysFailsPolicyV12

alwaysFailsAssetIdV1 :: C.AssetId
alwaysFailsAssetIdV1 = C.AssetId (policyIdV1 alwaysFailsPolicyV12) ""

alwaysFailsAssetIdV2 :: C.AssetId
alwaysFailsAssetIdV2 = C.AssetId alwaysFailsPolicyIdV2 ""

alwaysFailsPolicyTxInfoRedeemerV2 :: V2.Map ScriptPurpose Redeemer
alwaysFailsPolicyTxInfoRedeemerV2 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysFailsPolicyIdV2)
    (asRedeemer $ P.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysFailsMintWitnessV1
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV1 era Nothing =
  ( policyIdV1 alwaysFailsPolicyV12
  , mintScriptWitness
      era
      plutusL1
      (Left alwaysFailsPolicyScriptV1)
      (toScriptData ())
  )
alwaysFailsMintWitnessV1 era (Just refTxIn) =
  ( policyIdV1 alwaysFailsPolicyV12
  , mintScriptWitness era plutusL1 (Right refTxIn) (toScriptData ())
  )

alwaysFailsMintWitnessV1'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV1' era exunits =
  ( policyIdV1 alwaysFailsPolicyV12
  , mintScriptWitness'
      era
      plutusL1
      (Left alwaysFailsPolicyScriptV1)
      (toScriptData ())
      exunits
  )

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysFailsMintWitnessV2
  :: C.ShelleyBasedEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV2 era Nothing =
  ( policyIdV2 alwaysFailsPolicyV12
  , mintScriptWitness
      era
      plutusL2
      (Left alwaysFailsPolicyScriptV2)
      (toScriptData ())
  )
alwaysFailsMintWitnessV2 era (Just refTxIn) =
  ( policyIdV2 alwaysFailsPolicyV12
  , mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ())
  )

alwaysFailsMintWitnessV2'
  :: C.ShelleyBasedEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV2' sbe exunits =
  ( policyIdV2 alwaysFailsPolicyV12
  , mintScriptWitness'
      sbe
      plutusL2
      (Left alwaysFailsPolicyScriptV2)
      (toScriptData ())
      exunits
  )
