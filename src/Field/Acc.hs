module Field.Acc where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear

import Prelude as P

import Field

buildFieldAcc
  :: (Elt b, P.Fractional a)
  => FieldDescription a
  -> (V2 a -> V2 b)
  -> Array DIM3 (V2 b)
buildFieldAcc fd fg =
  let
    V2 resY resX = _fdRes fd
    coordinator = generateCoords fd
    aa' = _fdAA fd
    dim = Z :. resY :. resX :. aa'*aa'
  in
    fromFunction dim
    (\(Z :. y :. x :. i) -> fg . coordinator $ (V3 y x i))
