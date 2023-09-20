{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use underscore" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.BLS.Groth16.Common where

import Data.ByteString qualified as BS
import PlutusCore (DefaultUni)
import PlutusScripts.Helpers (bytesFromHex)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

---- Groth16 zk-SNARK ----

{- | An example of the on-chain computation required for verification of a Groth16
 proof.  The data here is derived from
 https://github.com/achimcc/groth16-example/blob/main/src/lib.rs
-}

-- type UProg   = UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()

-- Wrappers for compressed group elements for slightly better type safety
newtype CompressedG1Element = CompressedG1Element {compressedG1 :: P.BuiltinByteString}
  deriving newtype (PlutusTx.Lift DefaultUni)

mkG1Element :: BS.ByteString -> CompressedG1Element
mkG1Element = CompressedG1Element . P.toBuiltin . bytesFromHex

newtype CompressedG2Element = CompressedG2Element {compressedG2 :: P.BuiltinByteString}
  deriving newtype (PlutusTx.Lift DefaultUni)

mkG2Element :: BS.ByteString -> CompressedG2Element
mkG2Element = CompressedG2Element . P.toBuiltin . bytesFromHex

groth16Scalar :: Integer
groth16Scalar = 0x1884d0cbcc5947434e46d19b3e904e18a8ee8d0d39ce9d315f3b00e338c8f618

-- Lots of group elements for input to the computation

groth16alpha :: CompressedG1Element
groth16alpha =
  mkG1Element
    ( "b71db1fa5f41362e93025b3556d76ead1225cf590d1cdb9e"
        <> "382a1febb7963dcd24a51e18df04ab221becaf29169faf25"
    )

groth16beta :: CompressedG2Element
groth16beta =
  mkG2Element
    ( "b3a26b0b4712e78d5d71786d96132a7c585023a36632cada"
        <> "44171ac3f45db524c3f6570c8a3f7dec35ae1ac3309b05dd"
        <> "0b306db4f74fd9ec421ca70c54425d922eac4c403b00db91"
        <> "6fdedf065bdce00ece17b97a4e97173e4d5989818edfaa4c"
    )

groth16gamma :: CompressedG2Element
groth16gamma =
  mkG2Element
    ( "b5acb800cd49ed8cbddbf491a1fcf8abfc93f09d38bbb2ec"
        <> "b6b08e23a4642ce59c9b0386539ac3cecdfb66a9f027fc21"
        <> "0f259510756444bc5eef654f4d0612b5d6375f9526b1b966"
        <> "ce53b8f12594e1b399d08231cfe6c269a44aa8d587f2369d"
    )

groth16delta :: CompressedG2Element
groth16delta =
  mkG2Element
    ( "b3aa797bafa39a48f6f87c2483c894c281c807821c47301f"
        <> "fb755acfcfd22c2323cedf6349c7fedd3200a4ae558631e5"
        <> "01d299eb93135c07cf694ca118d1b386490529c60f57935c"
        <> "efa89fcafa13a83f84207b76fe078dc859d402743d468c15"
    )

groth16gamma_abc_1 :: CompressedG1Element
groth16gamma_abc_1 =
  mkG1Element
    ( "b7f6d06dd3e5246ef6b51b075c30b68fd490fbf85e0205f7"
        <> "9fa04d81133192139463b5e8efb22c39ef3dd1c5092015b8"
    )

groth16gamma_abc_2 :: CompressedG1Element
groth16gamma_abc_2 =
  mkG1Element
    ( "a2e637dbff52a1e4a8c5d985b3411fc5fd44af607e42923e"
        <> "abb47ad876e1f02b5be034adaf73952ae8affee5f51841de"
    )

groth16a :: CompressedG1Element
groth16a =
  mkG1Element
    ( "a05be50fab5795bb8784393a5045f98747173ad287f55e21"
        <> "3471bd559745551452453c4c3a39e7c88310849f3c7a1fc3"
    )

groth16b :: CompressedG2Element
groth16b =
  mkG2Element
    ( "ad6348b6b7b34c86bf37a748cd2d82a250dfc64846756688"
        <> "25a16f7da6a04d3424113e325ce734ec44956082c0a06e5f"
        <> "1868e1f1a6e559b9fe81f1a901f8a6341b301c45b25d3080"
        <> "fbc5039353d8f71b550b274ec4c07c70cd1153562c314c97"
    )

groth16c :: CompressedG1Element
groth16c =
  mkG1Element
    ( "b569cc491b4df035cbf49e951fd4fe30aa8236b0e2af68f4"
        <> "c1592cd40debeb718af33639db6bc1e2da9d98e553e5eaed"
    )

{-# INLINEABLE verifyBlsGroth16Script #-}
verifyBlsGroth16Script
  :: P.BuiltinByteString -- G1
  -> P.BuiltinByteString -- G2
  -> P.BuiltinByteString -- G2
  -> P.BuiltinByteString -- G2
  -> P.BuiltinByteString -- G1
  -> P.BuiltinByteString -- G1
  -> P.BuiltinByteString -- G1
  -> P.BuiltinByteString -- G2
  -> P.BuiltinByteString -- G1
  -> Integer
  -> sc
  -> Bool
verifyBlsGroth16Script
  (P.bls12_381_G1_uncompress -> alpha')
  (P.bls12_381_G2_uncompress -> beta')
  (P.bls12_381_G2_uncompress -> gamma')
  (P.bls12_381_G2_uncompress -> delta')
  (P.bls12_381_G1_uncompress -> abc1')
  (P.bls12_381_G1_uncompress -> abc2')
  (P.bls12_381_G1_uncompress -> a')
  (P.bls12_381_G2_uncompress -> b')
  (P.bls12_381_G1_uncompress -> c')
  s
  _sc =
    let l1 = P.bls12_381_millerLoop a' b'
        l2 = P.bls12_381_millerLoop alpha' beta'
        l3 = P.bls12_381_millerLoop c' delta'
        p = P.bls12_381_G1_add abc1' (P.bls12_381_G1_scalarMul s abc2')
        l4 = P.bls12_381_millerLoop p gamma'
        y = P.bls12_381_mulMlResult l2 (P.bls12_381_mulMlResult l3 l4)
     in P.bls12_381_finalVerify l1 y
