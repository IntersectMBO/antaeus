{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers.Tx where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Era qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Word (Word32)
import GHC.Stack qualified as GHC
import Hedgehog (MonadTest)
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (toMaryEraOnwards, toShelleyBasedEra)
import Helpers.Utils qualified as U

newtype SubmitError = SubmitError C.TxValidationErrorInCardanoMode
  deriving (Show)

notSupportedError :: (Show e) => e -> String
notSupportedError e = show e ++ " not supported"

-- | Check whether the auto-balancing txbody build (constructBalancedTx) resulted in an error
isTxBodyScriptExecutionError
  , isTxBodyError
  , isTxBodyErrorValidityInterval
  , isTxBodyErrorNonAdaAssetsUnbalanced
    :: String
    -> Either C.TxBodyErrorAutoBalance r
    -> Bool
isTxBodyScriptExecutionError expectedError (Left (C.TxBodyScriptExecutionError m)) = expectedError `isInfixOf` show m
isTxBodyScriptExecutionError _ _ = False
isTxBodyError expectedError (Left (C.TxBodyError m)) = expectedError `isInfixOf` show m
isTxBodyError _ _ = False
isTxBodyErrorValidityInterval expectedError (Left (C.TxBodyErrorValidityInterval m)) = expectedError `isInfixOf` show m
isTxBodyErrorValidityInterval _ _ = False
isTxBodyErrorNonAdaAssetsUnbalanced expectedError (Left (C.TxBodyErrorNonAdaAssetsUnbalanced m)) =
  expectedError `isInfixOf` show m
isTxBodyErrorNonAdaAssetsUnbalanced _ _ = False

isSubmitError :: String -> Either SubmitError () -> Bool
isSubmitError expectedError (Left (SubmitError error)) = expectedError `isInfixOf` show error
isSubmitError _ _ = False

-- | Build TxOut for spending or minting with no datum or reference script present
txOut
  :: C.CardanoEra era
  -> C.Value
  -> C.Address C.ShelleyAddr
  -> C.TxOut C.CtxTx era
txOut era value address = do
  let meo = toMaryEraOnwards era
      sbe = toShelleyBasedEra era
  C.TxOut
    (U.unsafeFromRight $ C.anyAddressInEra era $ C.toAddressAny address)
    (C.shelleyBasedEraConstraints sbe $ C.TxOutValueShelleyBased sbe (C.toLedgerValue meo value))
    C.TxOutDatumNone
    C.ReferenceScriptNone

-- | Build TxOut with a reference script
txOutWithRefScript
  :: C.CardanoEra era
  -> C.Value
  -> C.Address C.ShelleyAddr
  -> C.Script lang
  -> C.TxOut C.CtxTx era
txOutWithRefScript era value address script = withRefScript era script $ txOut era value address

txOutWithInlineDatum
  , txOutWithDatumHash
  , txOutWithDatumInTx
    :: C.CardanoEra era
    -> C.Value
    -> C.Address C.ShelleyAddr
    -> C.HashableScriptData
    -> C.TxOut C.CtxTx era

-- | Build TxOut with inline datum
txOutWithInlineDatum era value address datum = withInlineDatum era datum $ txOut era value address

-- | Build TxOut with datum hash
txOutWithDatumHash era value address datum = withDatumHash era datum $ txOut era value address

-- | Build TxOut with datum hash whilst including datum value in txbody
txOutWithDatumInTx era value address datum = withDatumInTx era datum $ txOut era value address

-- | Add reference script to TxOut
withRefScript
  :: C.CardanoEra era
  -> C.Script lang
  -> C.TxOut C.CtxTx era
  -> C.TxOut C.CtxTx era
withRefScript era script (C.TxOut e v d _) =
  C.TxOut
    e
    v
    d
    ( C.inEonForEra
        (error $ notSupportedError era)
        (\e -> C.ReferenceScript e (C.toScriptInAnyLang script))
        era
    )

withInlineDatum
  , withDatumHash
  , withDatumInTx
    :: C.CardanoEra era
    -> C.HashableScriptData
    -> C.TxOut C.CtxTx era
    -> C.TxOut C.CtxTx era

-- | Add inline datum to TxOut
withInlineDatum era datum (C.TxOut e v _ rs) =
  C.TxOut
    e
    v
    (C.inEonForEra (error $ notSupportedError era) (\e -> C.TxOutDatumInline e datum) era)
    rs

-- | Add datum hash to TxOut
withDatumHash era datum (C.TxOut e v _ rs) =
  C.TxOut
    e
    v
    ( C.inEonForEra
        (error $ notSupportedError era)
        (\e -> C.TxOutDatumHash e (C.hashScriptDataBytes datum))
        era
    )
    rs

-- | Add datum hash to TxOut whilst including datum value in txbody
withDatumInTx era datum (C.TxOut e v _ rs) =
  C.TxOut e v (C.inEonForEra (error $ notSupportedError era) (\e -> C.TxOutDatumInTx e datum) era) rs

-- | Empty transaction body to begin building from.
emptyTxBodyContent
  :: C.ShelleyBasedEra era -> C.LedgerProtocolParameters era -> C.TxBodyContent C.BuildTx era
emptyTxBodyContent sbe pparams = (C.defaultTxBodyContent sbe){C.txProtocolParams = C.BuildTxWith $ Just pparams}

txFee :: C.CardanoEra era -> C.Lovelace -> C.TxFee era
txFee era =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxFeeExplicit e) era

fromTxFeesExplicit :: Either imp exp -> exp
fromTxFeesExplicit (Left _) = error "Era must support explicit fees"
fromTxFeesExplicit (Right tfe) = tfe

txExtraKeyWits :: C.CardanoEra era -> [C.Hash C.PaymentKey] -> C.TxExtraKeyWitnesses era
txExtraKeyWits era pkh =
  C.inEonForEra
    (error $ notSupportedError era)
    (\e -> C.TxExtraKeyWitnesses e pkh)
    era

-- | Produce collateral inputs if era supports it. Used for building txbody.
txInsCollateral :: C.CardanoEra era -> [C.TxIn] -> C.TxInsCollateral era
txInsCollateral era txIns =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxInsCollateral e txIns) era

-- | Produce return collateral output if era supports it. Used for building txbody.
txReturnCollateral :: C.CardanoEra era -> C.TxOut C.CtxTx era -> C.TxReturnCollateral C.CtxTx era
txReturnCollateral era txIns =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxReturnCollateral e txIns) era

txTotalCollateral :: C.CardanoEra era -> C.Lovelace -> C.TxTotalCollateral era
txTotalCollateral era lovelace =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxTotalCollateral e lovelace) era

txScriptValidity :: C.CardanoEra era -> C.ScriptValidity -> C.TxScriptValidity era
txScriptValidity era validity =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxScriptValidity e validity) era

txValidityLowerBound :: C.CardanoEra era -> C.SlotNo -> C.TxValidityLowerBound era
txValidityLowerBound era slotNo =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxValidityLowerBound e slotNo) era

txValidityUpperBound :: C.CardanoEra era -> C.SlotNo -> C.TxValidityUpperBound era
txValidityUpperBound era slotNo =
  C.inEonForEra (error $ notSupportedError era) (\e -> C.TxValidityUpperBound e (Just slotNo)) era

{- | Get TxId from a signed transaction.
 Useful for producing TxIn for building subsequant transaction.
-}
txId :: C.Tx era -> C.TxId
txId = C.getTxId . C.getTxBody

{- | Build TxIn from TxId and index. Useful for waiting for or asserting expected TxOut is
  onchain after submitting transaction.
-}
txIn :: C.TxId -> Int -> C.TxIn
txIn txId txIx = C.TxIn txId (C.TxIx $ fromIntegral txIx)

pubkeyTxIns :: [C.TxIn] -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
pubkeyTxIns =
  map (\txIn -> txInWitness txIn $ C.KeyWitness C.KeyWitnessForSpending)

txInWitness
  :: C.TxIn
  -> C.Witness C.WitCtxTxIn era
  -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))
txInWitness txIn wit = (txIn, C.BuildTxWith wit)

txInsReference
  :: C.CardanoEra era
  -> [C.TxIn]
  -> C.TxInsReference build era
txInsReference era txIns = C.inEonForEra (error $ notSupportedError era) (\e -> C.TxInsReference e txIns) era

txMintValue
  :: C.CardanoEra era
  -> C.Value
  -> Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint era)
  -> C.TxMintValue C.BuildTx era
txMintValue era tv m = C.inEonForEra (error $ notSupportedError era) (\e -> C.TxMintValue e tv) era (C.BuildTxWith m)

txCertificates
  :: C.CardanoEra era
  -> [C.Certificate era]
  -> [C.StakeCredential]
  -> C.TxCertificates C.BuildTx era
txCertificates era certs stakeCreds =
  C.inEonForEra
    (error $ notSupportedError era)
    (\e -> C.TxCertificates e certs)
    era
    (C.BuildTxWith $ Map.fromList (stakeCreds `zip` repeat (C.KeyWitness C.KeyWitnessForStakeAddr)))

-- Takens the action ID and a map of voters and their votes, builds multiple VotingProcedures
-- and combines them into a single VotingProcedures
buildVotingProcedures
  :: C.ShelleyBasedEra era
  -> C.ConwayEraOnwards era
  -> C.TxId -- action id
  -> Word32
  -> [(L.Voter (C.EraCrypto (C.ShelleyLedgerEra era)), C.Vote)]
  -> C.VotingProcedures era
buildVotingProcedures sbe ceo txId txIx voters = C.shelleyBasedEraConstraints sbe $ do
  let gAID = C.createGovernanceActionId txId txIx
      votingProceduresList =
        voters
          <&> ( \(voter, vote) -> do
                  let voteProcedure = C.createVotingProcedure ceo vote Nothing
                  C.singletonVotingProcedures ceo voter gAID (C.unVotingProcedure voteProcedure)
              )
  foldr1 C.unsafeMergeVotingProcedures votingProceduresList

buildTx
  :: (MonadIO m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.TxBodyContent C.BuildTx era
  -> C.Address C.ShelleyAddr
  -> C.ShelleyWitnessSigningKey
  -> m (C.Tx era)
buildTx era localNodeConnectInfo txBody changeAddress sKey =
  buildTxWithAnyWitness era localNodeConnectInfo txBody changeAddress [sKey]

buildTxWithAnyWitness
  :: (MonadIO m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.TxBodyContent C.BuildTx era
  -> C.Address C.ShelleyAddr
  -> [C.ShelleyWitnessSigningKey]
  -> m (C.Tx era)
buildTxWithAnyWitness era localNodeConnectInfo txBody changeAddress sKeys =
  buildTxWithWitnessOverride era localNodeConnectInfo txBody changeAddress Nothing sKeys

buildTxWithWitnessOverride
  :: (MonadIO m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.TxBodyContent C.BuildTx era
  -> C.Address C.ShelleyAddr
  -> Maybe Word
  -> [C.ShelleyWitnessSigningKey]
  -> m (C.Tx era)
buildTxWithWitnessOverride era localNodeConnectInfo txBody changeAddress mWitnessOverride sKeys =
  fromEither <$> buildTxWithError era localNodeConnectInfo txBody changeAddress mWitnessOverride sKeys
  where
    fromEither (Left e) = error $ show e
    fromEither (Right tx) = tx

{- | Maybe build signed transaction using convenience functions for calculating fees and exunits.
  Useful for asserting for error.
-}
buildTxWithError
  :: (MonadIO m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.TxBodyContent C.BuildTx era
  -> C.Address C.ShelleyAddr
  -> Maybe Word
  -> [C.ShelleyWitnessSigningKey]
  -> m (Either C.TxBodyErrorAutoBalance (C.Tx era))
buildTxWithError era localNodeConnectInfo txBody changeAddress mWitnessOverride sKeys = do
  let certs = do
        case C.txCertificates txBody of
          C.TxCertificatesNone -> []
          C.TxCertificates _ certs _ -> certs

  localStateQueryResult <-
    liftIO
      ( C.executeLocalStateQueryExpr localNodeConnectInfo Nothing $
          C.queryStateForBalancedTx era allInputs certs
      )

  let ( nodeEraUtxo
        , ledgerPParams
        , eraHistory
        , systemStart
        , stakePools
        , stakeDelegDeposits
        , drepDelegDeposits
        ) =
          U.unsafeFromRight $ U.unsafeFromRight localStateQueryResult
      sbe = toShelleyBasedEra era

  return $
    C.constructBalancedTx
      sbe
      txBody
      (C.shelleyAddressInEra sbe changeAddress)
      mWitnessOverride -- Override key witnesses
      nodeEraUtxo -- tx inputs
      ledgerPParams
      (C.toLedgerEpochInfo eraHistory)
      systemStart
      stakePools
      stakeDelegDeposits
      drepDelegDeposits
      sKeys
  where
    allInputs :: [C.TxIn]
    allInputs = do
      let
        txIns = fst <$> C.txIns txBody
        colTxIns = case C.txInsCollateral txBody of
          C.TxInsCollateralNone -> []
          C.TxInsCollateral _ colTxIns -> colTxIns
        refTxIns = case C.txInsReference txBody of
          C.TxInsReferenceNone -> []
          C.TxInsReference _ refTxIns -> refTxIns

      txIns ++ colTxIns ++ refTxIns

-- | Build txbody with no calculated change, fees or execution unit
buildRawTx
  :: (MonadTest m)
  => C.ShelleyBasedEra era
  -> C.TxBodyContent C.BuildTx era
  -> m (C.TxBody era)
buildRawTx sbe = HE.leftFail . C.createAndValidateTransactionBody sbe -- TODO: handle error

-- | Witness txbody with signing key when not using convenience build function
signTx
  :: (MonadIO m)
  => C.ShelleyBasedEra era
  -> C.TxBody era
  -> C.ShelleyWitnessSigningKey
  -> m (C.KeyWitness era)
signTx era txbody witness =
  return $ C.makeShelleyKeyWitness era txbody witness

submitTx
  :: (MonadIO m, MonadTest m)
  => C.ShelleyBasedEra era
  -> C.LocalNodeConnectInfo
  -> C.Tx era
  -> m ()
submitTx sbe localNodeConnectInfo tx = do
  submitResult :: C.SubmitResult era <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode sbe tx
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadTest m) => C.SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      C.SubmitFail reason -> H.failMessage GHC.callStack $ "Transaction failed: " <> show reason
      C.SubmitSuccess -> pure ()

submitTx'
  :: (MonadIO m, MonadTest m)
  => C.ShelleyBasedEra era
  -> C.LocalNodeConnectInfo
  -> C.Tx era
  -> m (Either SubmitError ())
submitTx' sbe localNodeConnectInfo tx = do
  submitResult :: C.SubmitResult era <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode sbe tx
  returnErrorOnTxSubmitFail submitResult
  where
    returnErrorOnTxSubmitFail
      :: (MonadTest m) => C.SubmitResult C.TxValidationErrorInCardanoMode -> m (Either SubmitError ())
    returnErrorOnTxSubmitFail = \case
      C.SubmitFail reason -> pure $ Left $ SubmitError reason
      C.SubmitSuccess -> pure $ Right ()
