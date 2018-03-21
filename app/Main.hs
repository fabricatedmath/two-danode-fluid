{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Linear

import Data.Array.Accelerate.Data.Colour.RGB            as RGB

import Pipes hiding (lift)
import Pipes.Safe
import Pipes.Graphics.Accelerate

import Prelude as P

import Acc.Lib

import Field
import Field.Hint
import Field.Hint.Acc

import Fluid

import Type

defaultFD :: P.Num a => DIM2 -> FieldDescription a
defaultFD (Z :. ydim :. xdim) = FromCenter (V2 ydim xdim) (V2 0 0) (2) (1)

defaultFS :: FieldStrings
defaultFS = Cartesian "sin(13*x)*cos(10*y*x)" "sin(5*x)*cos (10*y*x)"
--defaultFS = Cartesian "x" "y"

main :: IO ()
main =
  do
    let
      ydim = 1000
      xdim = 1000
      dim = Z :. ydim :. xdim :: DIM2
    result <- buildPhaseSpace (defaultFD dim) defaultFS
    case result of
      Left err -> print err
      Right v ->
        let
          !idf = makeDensity_hsv dim
          !ivf =
            run1
            (A.map
              (\vec ->
                 let (V2 y x) = unlift vec :: V2 (Exp Float)
                 in lift (y/1000,x/1000)
              ) . A.sum
            ) v
        in runSafeT $ runEffect $ fluidProducer idf ivf >-> openGLConsumer' dim

fluidProducer
  :: Monad m
  => Array DIM2 (Float, RGB Float)
  -> Array DIM2 (Float,Float)
  -> Producer' (Array DIM2 Word32) m ()
fluidProducer idf ivf = f (idf,ivf)
  where
    step =
      run1
      (\arr ->
          let e = fluid 100 0.01 0 0 arr
              (df',vf') = unlift e :: (Acc (Field RGBDensity), Acc VelocityField)
              cf' = A.map packRGBTupA $ makePicture df'
              vf'' = A.zipWith (.+.) (use ivf) $ decayVelocity 0.9 vf'
          in
            lift (df', vf'', cf')
      )

    f (df,vf) =
      do
        let (df',vf',cf') = step (df,idf,vf)
        yield cf'
        f (df',vf')
