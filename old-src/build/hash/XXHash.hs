{-# LANGUAGE ForeignFunctionInterface,
GHCForeignImportPrim,
MagicHash,
UnboxedTuples,
UnliftedFFITypes #-}

import GHC.Exts
import GHC.Word

{-# NOINLINE test #-}
test :: Word# -> (# Word#, Word# #)
test a = (# case W64# a + W64# a of W64# b -> b ,
            case W64# a * W64# a of W64# b -> b
         #)

foreign import prim "test2"
  test2 :: Word# -> (# Word#, Word# #)

main = do
  print $ case (case 2 of W64# a -> test a) of (# b,c #) -> (W64# b, W64# c)
  print $ case (case 2 of W64# a -> test2 a) of (# b,c #) -> (W64# b, W64# c)