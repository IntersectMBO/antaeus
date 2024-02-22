{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Helpers.Query where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Conway.Governance qualified as C
import Cardano.Ledger.SafeHash qualified as C
import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isInfixOf, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Hedgehog (MonadTest)
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (toShelleyBasedEra)
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as O

-- | Find the first UTxO at address and return as TxIn. Used for txbody's txIns.
firstTxIn
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> m C.TxIn
firstTxIn era = txInAtAddressByIndex era 0

-- | Find UTxO at address by index and return as TxIn. Used for txbody's txIns.
txInAtAddressByIndex
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> Int
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> m C.TxIn
txInAtAddressByIndex era idx localNodeConnectInfo address = do
  atM idx =<< txInsFromUtxo =<< findUTxOByAddress era localNodeConnectInfo address
  where
    atM :: (MonadTest m) => Int -> [a] -> m a
    atM i' l = return $ l !! i'

-- | Find the TxIn at address which is ada-only and has the most ada
adaOnlyTxInAtAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> m C.TxIn
adaOnlyTxInAtAddress era localNodeConnectInfo address = do
  utxo <- findUTxOByAddress era localNodeConnectInfo address
  return $ fst $ head $ sortByMostAda $ adaOnly $ Map.toList $ C.unUTxO utxo
  where
    adaOnly =
      filter
        ( \(_, C.TxOut _ (C.TxOutValueShelleyBased sbe v) _ _) ->
            ((length $ C.valueToList (C.fromLedgerValue sbe v)) == 1)
              && ((fst $ head $ C.valueToList (C.fromLedgerValue sbe v)) == C.AdaAssetId)
        )
    sortByMostAda =
      sortBy
        ( \(_, C.TxOut _ (C.TxOutValueShelleyBased sbe v1) _ _)
           (_, C.TxOut _ (C.TxOutValueShelleyBased _ v2) _ _) ->
              compare
                (snd $ head $ C.valueToList (C.fromLedgerValue sbe v2))
                (snd $ head $ C.valueToList (C.fromLedgerValue sbe v1))
        )

-- | Get TxIns from all UTxOs
txInsFromUtxo :: (MonadIO m) => C.UTxO era -> m [C.TxIn]
txInsFromUtxo utxos = do
  let (txIns, _) = unzip $ Map.toList $ C.unUTxO utxos
  return txIns

-- | Query ledger for UTxOs at address
findUTxOByAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address a
  -> m (C.UTxO era)
findUTxOByAddress era localNodeConnectInfo address =
  let query =
        C.QueryInShelleyBasedEra (toShelleyBasedEra era) $
          C.QueryUTxO $
            C.QueryUTxOByAddress $
              Set.singleton (C.toAddressAny address)
   in H.leftFailM . H.leftFailM . liftIO $
        C.queryNodeLocalState localNodeConnectInfo O.VolatileTip $
          C.QueryInEra query

-- | Get [TxIn] and total lovelace value for an address.
getAddressTxInsLovelaceValue
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address a
  -> m ([C.TxIn], C.Lovelace)
getAddressTxInsLovelaceValue era con address = do
  utxo <- findUTxOByAddress era con address
  let (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
      values = map (\case C.TxOut _ v _ _ -> C.txOutValueToLovelace v) txOuts
  pure (txIns, sum values)

-- | Get [TxIn] and value for an address (including assets).
getAddressTxInsValue
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address a
  -> m ([C.TxIn], C.Value)
getAddressTxInsValue era con address = do
  utxo <- findUTxOByAddress era con address
  let (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
      values = map (\case C.TxOut _ v _ _ -> C.txOutValueToValue v) txOuts
  pure (txIns, (mconcat values))

-- TODO: loop timeout
waitForTxIdAtAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> C.TxId
  -> m ()
waitForTxIdAtAddress era localNodeConnectInfo address txId = do
  let loop = do
        txIns <- txInsFromUtxo =<< findUTxOByAddress era localNodeConnectInfo address
        let txIds = map (\(C.TxIn txId _) -> txId) txIns
        when (not $ txId `elem` txIds) loop
  loop

waitForTxInAtAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure
  -> m ()
waitForTxInAtAddress era localNodeConnectInfo address txIn debugStr = do
  let timeoutSeconds = 90 :: Int
      loop i prevUtxo = do
        if i == 0
          then
            error
              ( "waitForTxInAtAddress timeout. \n-- Debug --\nTest function: "
                  ++ debugStr
                  ++ "\nAddress: "
                  ++ show address
                  ++ "\nTxIn: "
                  ++ show txIn
                  ++ "\nPrev UTxO: "
                  ++ show prevUtxo
              )
          else HE.threadDelay 1000000
        utxos <- findUTxOByAddress era localNodeConnectInfo address
        when (Map.notMember txIn $ C.unUTxO utxos) (loop (pred i) (show utxos))
  loop timeoutSeconds ""

-- | Get tx out at address is for general use when txo is expected
getTxOutAtAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure (waitForTxInAtAddress)
  -> m (C.TxOut C.CtxUTxO era)
getTxOutAtAddress era localNodeConnectInfo address txIn debugStr = do
  maybeTxOut <- getTxOutAtAddress' era localNodeConnectInfo address txIn debugStr
  return $ fromMaybe maybeTxOut
  where
    fromMaybe Nothing = error $ "txIn " ++ show txIn ++ " is not at address " ++ show address
    fromMaybe (Just txo) = txo

-- | Maybe get tx out at address for asserting when it is not expected to be present
getTxOutAtAddress'
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure (waitForTxInAtAddress)
  -> m (Maybe (C.TxOut C.CtxUTxO era))
getTxOutAtAddress' era localNodeConnectInfo address txIn debugStr = do
  waitForTxInAtAddress era localNodeConnectInfo address txIn debugStr
  utxos <- findUTxOByAddress era localNodeConnectInfo address
  return $ Map.lookup txIn $ C.unUTxO utxos

isTxOutAtAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> m Bool
isTxOutAtAddress era localNodeConnectInfo address txIn = do
  utxos <- findUTxOByAddress era localNodeConnectInfo address
  return $ Map.member txIn $ C.unUTxO utxos

txOutHasValue
  :: (MonadIO m)
  => C.TxOut C.CtxUTxO era
  -> C.Value
  -> m Bool
txOutHasValue (C.TxOut _ txOutValue _ _) tokenValue = do
  let value = C.txOutValueToValue txOutValue
  return $ isInfixOf (C.valueToList tokenValue) (C.valueToList value)

-- | Query network's protocol parameters
getProtocolParams
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m (C.LedgerProtocolParameters era)
getProtocolParams era localNodeConnectInfo = do
  lpp <-
    H.leftFailM . H.leftFailM . liftIO $
      C.queryNodeLocalState localNodeConnectInfo O.VolatileTip $
        C.QueryInEra $
          C.QueryInShelleyBasedEra (toShelleyBasedEra era) C.QueryProtocolParameters
  return $ C.LedgerProtocolParameters lpp

-- | Query current epoch
getCurrentEpoch
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m C.EpochNo
getCurrentEpoch era localNodeConnectInfo =
  H.leftFailM . H.leftFailM . liftIO $
    C.queryNodeLocalState localNodeConnectInfo O.VolatileTip $
      C.QueryInEra $
        C.QueryInShelleyBasedEra (toShelleyBasedEra era) C.QueryEpoch

waitForNextEpoch
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> String -- temp debug text for intermittent timeout failure
  -> C.EpochNo
  -> m C.EpochNo
waitForNextEpoch era localNodeConnectInfo debugStr prevEpochNo = go (120 :: Int) -- 120 second timeout
  where
    go 0 = error $ "waitForNextEpoch timeout. \n-- Debug --\nTest function: " ++ debugStr
    go i = do
      currentEpochNo <- getCurrentEpoch era localNodeConnectInfo
      case currentEpochNo - prevEpochNo of
        0 -> do
          liftIO $ threadDelay 1000000 -- 1s
          go (pred i)
        1 -> return currentEpochNo
        diff
          | diff > 1 -> error "Current epoch is more than 1 epoch beyond the previous epoch"
          | otherwise -> error "Current epoch is less than the previous epoch"

waitForNextEpoch_
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> String -- temp debug text for intermittent timeout failure
  -> C.EpochNo
  -> m ()
waitForNextEpoch_ e l n s = void $ waitForNextEpoch e l n s

-- | Query current constitution hash
getConstitution
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m (Maybe (C.Constitution (C.ShelleyLedgerEra era)))
getConstitution era localNodeConnectInfo =
  H.leftFailM . H.leftFailM . liftIO $
    C.queryNodeLocalState localNodeConnectInfo O.VolatileTip $
      C.QueryInEra $
        C.QueryInShelleyBasedEra (toShelleyBasedEra era) C.QueryConstitution

getConstitutionAnchor
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m (C.Anchor (L.EraCrypto (C.ShelleyLedgerEra era)))
getConstitutionAnchor era localNodeConnectInfo =
  C.constitutionAnchor . fromJust <$> getConstitution era localNodeConnectInfo

getConstitutionAnchorUrl
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m L.Url
getConstitutionAnchorUrl era localNodeConnectInfo =
  C.anchorUrl . C.constitutionAnchor . fromJust <$> getConstitution era localNodeConnectInfo

getConstitutionAnchorHash
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m (C.SafeHash (L.EraCrypto (C.ShelleyLedgerEra era)) C.AnchorData)
getConstitutionAnchorHash era localNodeConnectInfo =
  C.anchorDataHash . C.constitutionAnchor . fromJust
    <$> getConstitution era localNodeConnectInfo

getConstitutionAnchorHashAsString
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo
  -> m String
getConstitutionAnchorHashAsString era localNodeConnectInfo =
  show . L.extractHash <$> getConstitutionAnchorHash era localNodeConnectInfo
