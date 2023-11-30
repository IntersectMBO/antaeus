{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Helpers.TestData where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock.POSIX (POSIXTime)
import Hedgehog (MonadTest)
import Helpers.Testnet qualified as TN

type TestFunction era =
  forall m
   . (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)

data TestInfo era = TestInfo
  { testName :: String
  , testDescription :: String
  , test :: TestFunction era
  }

instance Show (TestInfo era) where
  show (TestInfo name description _) =
    "TestInfo { testName = "
      ++ show name
      ++ ", testDescription = "
      ++ show description
      ++ " }"

data TestParams era = TestParams
  { localNodeConnectInfo :: C.LocalNodeConnectInfo
  , pparams :: C.LedgerProtocolParameters era
  , networkId :: C.NetworkId
  , tempAbsPath :: FilePath
  , mTime :: Maybe POSIXTime
  }
