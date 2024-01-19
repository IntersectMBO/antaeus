{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.WriteScriptFiles where

import Control.Monad.IO.Class (MonadIO, liftIO)
import PlutusScripts.Basic.V_1_1 qualified as PS_1_1
import PlutusScripts.SECP256k1.V_1_1 qualified as PS_1_1

writeV3ScriptFiles :: (MonadIO m) => m ()
writeV3ScriptFiles = liftIO $ do
  PS_1_1.writeAlwaysSucceedPolicyScriptV3
  PS_1_1.writeAlwaysSucceedSpendScriptV3
  PS_1_1.writeAlwaysFailsPolicyScriptV3
  PS_1_1.writeVerifySchnorrPolicyScriptV3
  PS_1_1.writeVerifyEcdsaPolicyScriptV3
  PS_1_1.writeTokenNamePolicyScriptV3
  PS_1_1.writeTimeRangePolicyScriptV3
  PS_1_1.writeWitnessRedeemerPolicyScriptV3
