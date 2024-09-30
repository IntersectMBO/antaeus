{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.Governance.Common where

import PlutusTx.Prelude

import Cardano.Api qualified as C
import PlutusLedgerApi.V3 (
  GovernanceAction (
    HardForkInitiation,
    InfoAction,
    NewConstitution,
    NoConfidence,
    ParameterChange,
    TreasuryWithdrawals,
    UpdateCommittee
  ),
  GovernanceActionId,
  Lovelace,
  Map,
  ProposalProcedure (ppDeposit, ppGovernanceAction, ppReturnAddr),
  ScriptContext (scriptContextScriptInfo, scriptContextTxInfo),
  ScriptInfo,
  ScriptPurpose,
  TxCert,
  TxInfo (
    txInfoCurrentTreasuryAmount,
    txInfoProposalProcedures,
    txInfoTreasuryDonation,
    txInfoTxCerts,
    txInfoVotes
  ),
  Value,
  Vote,
  Voter,
 )
import PlutusScripts.Helpers (toScriptData)
import PlutusTx.AssocMap qualified as AM

-- TODO: compare cost of this implementation with PlutusTx's length
{-# INLINEABLE _lengthEq #-}
_lengthEq :: [a] -> [b] -> Bool
_lengthEq [] [] = True
_lengthEq (_ : xs) (_ : ys) = _lengthEq xs ys
_lengthEq _ _ = False

{-# INLINEABLE listEq #-}
listEq :: (Eq a) => [a] -> [a] -> Bool
listEq rs cs = length rs == length cs && and (zipWith (==) rs cs)

-- ScriptPurpose --

{-# INLINEABLE mkVerifyScriptPurpose #-}
mkVerifyScriptPurpose :: ScriptContext -> (ScriptInfo -> Bool) -> Bool
mkVerifyScriptPurpose sc = ($ scriptContextScriptInfo sc)

scriptPurposeAssetName :: C.AssetName
scriptPurposeAssetName = C.AssetName "ScriptPurpose"

verifyScriptPurposeRedeemer :: ScriptPurpose -> C.HashableScriptData
verifyScriptPurposeRedeemer = toScriptData

-- TxCert --

{-# INLINEABLE mkVerifyTxCerts #-}
mkVerifyTxCerts :: [TxCert] -> ScriptContext -> Bool
mkVerifyTxCerts r sc = r == (txInfoTxCerts $ scriptContextTxInfo sc)

txCertsAssetName :: C.AssetName
txCertsAssetName = C.AssetName "TxCerts"

verifyTxCertsRedeemer :: [TxCert] -> C.HashableScriptData
verifyTxCertsRedeemer = toScriptData

-- txInfoVotes --

{-# INLINEABLE mkVerifyVotes #-}
mkVerifyVotes :: Map Voter (Map GovernanceActionId Vote) -> ScriptContext -> Bool
mkVerifyVotes r sc = do
  let redeemerVoters = AM.keys r
      contextVoters = AM.keys $ txInfoVotes $ scriptContextTxInfo sc
      redeemerGovActionIds = AM.keys <$> AM.elems r
      contextGovActionIds = AM.keys <$> AM.elems r
      redeemerVotes = AM.elems <$> AM.elems r
      contextVotes = AM.elems <$> AM.elems r
  listEq redeemerVoters contextVoters
    && emListEq redeemerGovActionIds contextGovActionIds
    && emListEq redeemerVotes contextVotes
  where
    {-# INLINEABLE emListEq #-}
    emListEq :: (Eq a) => [[a]] -> [[a]] -> Bool
    emListEq rs cs =
      (length rs == length cs)
        -- lengthEq rs cs -- alternate implementation
        && and (zipWith listEq rs cs)

votesAssetName :: C.AssetName
votesAssetName = C.AssetName "Votes"

verifyVotesRedeemer
  :: Map Voter (Map GovernanceActionId Vote) -> C.HashableScriptData
verifyVotesRedeemer = toScriptData

-- txInfoProposalProcedures --

{-# INLINEABLE mkVerifyProposalProcedures #-}
mkVerifyProposalProcedures :: [ProposalProcedure] -> ScriptContext -> Bool
mkVerifyProposalProcedures expectedPProcedures ctx =
  (length expectedPProcedures == length ctxPProcedures)
    && all
      (uncurry eqProposalProcedure)
      (zip expectedPProcedures ctxPProcedures)
  where
    ctxPProcedures = txInfoProposalProcedures (scriptContextTxInfo ctx)

    eqProposalProcedure :: ProposalProcedure -> ProposalProcedure -> Bool
    eqProposalProcedure l r =
      (ppDeposit l == ppDeposit r)
        && (ppReturnAddr l == ppReturnAddr r)
        && eqGovernanceAction
          (ppGovernanceAction l)
          (ppGovernanceAction r)

    eqGovernanceAction :: GovernanceAction -> GovernanceAction -> Bool
    eqGovernanceAction l r =
      case (l, r) of
        (ParameterChange a ps sh, ParameterChange a' ps' sh') ->
          (a == a') && (ps == ps') && (sh == sh')
        (HardForkInitiation a pv, HardForkInitiation a' pv') ->
          (a == a') && (pv == pv')
        (TreasuryWithdrawals m sh, TreasuryWithdrawals m' sh') ->
          (toList m == toList m') && (sh == sh')
        (NoConfidence a, NoConfidence a') -> a == a'
        (UpdateCommittee a rm am q, UpdateCommittee a' rm' am' q') ->
          (a == a') && (rm == rm') && (toList am == toList am') && (q == q')
        (NewConstitution a c, NewConstitution a' c') ->
          (a == a') && (c == c')
        (InfoAction, InfoAction) -> True
        _ -> False

proposalProceduresAssetName :: C.AssetName
proposalProceduresAssetName = C.AssetName "ProposalProcedures"

verifyProposalProceduresRedeemer :: [ProposalProcedure] -> C.HashableScriptData
verifyProposalProceduresRedeemer = toScriptData

-- txInfoCurrentTreasuryAmount --

{-# INLINEABLE mkVerifyCurrentTreasuryAmount #-}
mkVerifyCurrentTreasuryAmount :: Maybe Lovelace -> ScriptContext -> Bool
mkVerifyCurrentTreasuryAmount r sc =
  r == (txInfoCurrentTreasuryAmount $ scriptContextTxInfo sc)

currentTreasuryAmountAssetName :: C.AssetName
currentTreasuryAmountAssetName = C.AssetName "CurrentTreasuryAmount"

currentTreasuryAmountRedeemer :: Maybe Value -> C.HashableScriptData
currentTreasuryAmountRedeemer = toScriptData

-- txInfoTreasuryDonation --

{-# INLINEABLE mkVerifyTreasuryDonation #-}
mkVerifyTreasuryDonation :: Maybe Lovelace -> ScriptContext -> Bool
mkVerifyTreasuryDonation r sc = r == (txInfoTreasuryDonation $ scriptContextTxInfo sc)

treasuryDonationAssetName :: C.AssetName
treasuryDonationAssetName = C.AssetName "TreasuryDonationAssetName"

treasuryDonationRedeemer :: Maybe Value -> C.HashableScriptData
treasuryDonationRedeemer = toScriptData
