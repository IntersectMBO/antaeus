{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Governance.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.Governance.Common (
  currentTreasuryAmountAssetName,
  mkVerifyCurrentTreasuryAmount,
  mkVerifyProposalProcedures,
  mkVerifyScriptPurpose,
  mkVerifyTreasuryDonation,
  mkVerifyTxCerts,
  mkVerifyVotes,
  proposalProceduresAssetName,
  scriptPurposeAssetName,
  treasuryDonationAssetName,
  txCertsAssetName,
  votesAssetName,
 )
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
  toScriptData,
 )
import PlutusTx qualified

-- ScriptPurpose --

verifyScriptPurposePolicy :: SerialisedScript
verifyScriptPurposePolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVerifyScriptPurpose||])

verifyScriptPurposeScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyScriptPurposeScriptV3 = C.PlutusScriptSerialised verifyScriptPurposePolicy

verifyScriptPurposeAssetIdV3 :: C.AssetId
verifyScriptPurposeAssetIdV3 =
  C.AssetId (policyIdV3 verifyScriptPurposePolicy) scriptPurposeAssetName

verifyScriptPurposeMintWitnessV3
  :: C.ShelleyBasedEra era
  -> V3.ScriptPurpose
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyScriptPurposeMintWitnessV3 sbe redeemer =
  ( policyIdV3 verifyScriptPurposePolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyScriptPurposeScriptV3)
      (toScriptData redeemer)
  )

-- TxCert --

verifyTxCertsPolicy :: SerialisedScript
verifyTxCertsPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVerifyTxCerts||])

verifyTxCertsScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyTxCertsScriptV3 = C.PlutusScriptSerialised verifyTxCertsPolicy

verifyTxCertsAssetIdV3 :: C.AssetId
verifyTxCertsAssetIdV3 =
  C.AssetId (policyIdV3 verifyTxCertsPolicy) txCertsAssetName

verifyTxCertsMintWitnessV3
  :: C.ShelleyBasedEra era
  -> [V3.TxCert]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyTxCertsMintWitnessV3 sbe redeemer =
  ( policyIdV3 verifyTxCertsPolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyTxCertsScriptV3)
      (toScriptData redeemer)
  )

-- txInfoVotingProcedures --

verifyVotesPolicy :: SerialisedScript
verifyVotesPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVerifyVotes||])

verifyVotescriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyVotescriptV3 = C.PlutusScriptSerialised verifyVotesPolicy

verifyVotesAssetIdV3 :: C.AssetId
verifyVotesAssetIdV3 = C.AssetId (policyIdV3 verifyVotesPolicy) votesAssetName

verifyVotesMintWitnessV3
  :: C.ShelleyBasedEra era
  -> V3.Map V3.Voter (V3.Map V3.GovernanceActionId V3.Vote)
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyVotesMintWitnessV3 sbe redeemer =
  ( policyIdV3 verifyVotesPolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyVotescriptV3)
      (toScriptData redeemer)
  )

-- txInfoProposalProcedures --

verifyProposalProceduresPolicy :: SerialisedScript
verifyProposalProceduresPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVerifyProposalProcedures||])

verifyProposalProcedureScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyProposalProcedureScriptV3 =
  C.PlutusScriptSerialised verifyProposalProceduresPolicy

verifyProposalProceduresAssetIdV3 :: C.AssetId
verifyProposalProceduresAssetIdV3 =
  C.AssetId
    (policyIdV3 verifyProposalProceduresPolicy)
    proposalProceduresAssetName

verifyProposalProceduresMintWitnessV3
  :: C.ShelleyBasedEra era
  -> [V3.ProposalProcedure]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyProposalProceduresMintWitnessV3 sbe redeemer =
  ( policyIdV3 verifyProposalProceduresPolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyProposalProcedureScriptV3)
      (toScriptData redeemer)
  )

-- txInfoCurrentTreasuryAmount --

verifyCurrentTreasuryAmountPolicy :: SerialisedScript
verifyCurrentTreasuryAmountPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVerifyCurrentTreasuryAmount||])

verifyCurrentTreasuryAmountScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyCurrentTreasuryAmountScriptV3 =
  C.PlutusScriptSerialised verifyCurrentTreasuryAmountPolicy

verifyCurrentTreasuryAmountPolicyAssetIdV3 :: C.AssetId
verifyCurrentTreasuryAmountPolicyAssetIdV3 =
  C.AssetId
    (policyIdV3 verifyCurrentTreasuryAmountPolicy)
    currentTreasuryAmountAssetName

verifyCurrentTreasuryAmountMintWitnessV3
  :: C.ShelleyBasedEra era
  -> [V3.ProposalProcedure]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyCurrentTreasuryAmountMintWitnessV3 sbe redeemer =
  ( policyIdV3 verifyCurrentTreasuryAmountPolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyCurrentTreasuryAmountScriptV3)
      (toScriptData redeemer)
  )

-- txInfoTreasuryDonation --

verifyTreasuryDonationPolicy :: SerialisedScript
verifyTreasuryDonationPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVerifyTreasuryDonation||])

verifyTreasuryDonationScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyTreasuryDonationScriptV3 =
  C.PlutusScriptSerialised verifyTreasuryDonationPolicy

verifyTreasuryDonationAssetIdV3 :: C.AssetId
verifyTreasuryDonationAssetIdV3 =
  C.AssetId (policyIdV3 verifyTreasuryDonationPolicy) treasuryDonationAssetName

verifyTreasuryDonationMintWitnessV3
  :: C.ShelleyBasedEra era
  -> [V3.ProposalProcedure]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyTreasuryDonationMintWitnessV3 sbe redeemer =
  ( policyIdV3 verifyTreasuryDonationPolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyTreasuryDonationScriptV3)
      (toScriptData redeemer)
  )
