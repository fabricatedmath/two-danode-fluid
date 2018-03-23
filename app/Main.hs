{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens hiding (use)
import Control.Monad (forever, forM_)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Linear

import Data.Array.Accelerate.Data.Colour.RGB            as RGB

import Pipes hiding (lift)
import qualified Pipes.Prelude as Pipes (take)
import Pipes.Safe
import Pipes.Graphics
import Pipes.Graphics.Accelerate

import Prelude as P

import Acc.Lib

import Config

import Field
import Field.Hint
import Field.Hint.Acc

import Fluid

import Type

defaultFD :: P.Num a => DIM2 -> FieldDescription a
defaultFD (Z :. ydim :. xdim) = FromCenter (V2 ydim xdim) (V2 0 0) (20) (1)

defaultFS :: FieldStrings --043 (/ 100)
defaultFS =
  Cartesian
  "(abs $ cos (0.75*x))**(abs $ cos (3*y)) + sin (3*x)"
  "sin x - cos y"

printer :: MonadIO m => Pipe a a m ()
printer =
  forM_ [1..]
  (\i ->
      do
        await >>= yield
        if i `mod` 100 P.== 0 then liftIO $ print i else return ()
  )

main :: IO ()
main =
  do
    descr <- loadConfigFromArgs
    let
      V2 ydim xdim = descr ^. descrFD.fdRes
      fd = descr ^. descrFD
      fs = descr ^. descrFS
      dim = Z :. ydim :. xdim :: DIM2
    result <- buildPhaseSpace fd fs
    case result of
      Left err -> print err
      Right v ->
        let
          !idf = makeDensity_hsv dim
          m' =
            run1
            (A.maximum . A.flatten . A.map quadrance) v :: Array DIM0 Float
          m = indexArray m' Z
          !ivf =
            run1
            (A.map
              (\vec ->
                 let (V2 y x) = unlift vec :: V2 (Exp Float)
                 in lift (y/100,x/100)
              ) . A.sum
            ) v
        in
          do
            print m
            runSafeT $ runEffect $
              fluidProducer idf ivf >->
              printer >->
              forever (await >>= yield . arrayToImage) >->
              Pipes.take 5000 >->
              pngWriter 5 "/home/cdurham/Desktop/video/v"
              --squaredDistanceShutoff >->
              --openGLConsumer dim

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
