{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens hiding (use)
import Control.Monad (forM_, forever)

import Data.Array.Accelerate as A hiding ((>->))
#ifdef NATIVE
import Data.Array.Accelerate.LLVM.Native
#else
import Data.Array.Accelerate.LLVM.PTX
#endif
import Data.Array.Accelerate.Linear

import Data.Array.Accelerate.Data.Colour.RGB            as RGB

import Pipes hiding (lift)
import qualified Pipes.Prelude as Pipes (take)
import Pipes.Safe
import Pipes.Graphics (pngWriter)
import Pipes.Graphics.Accelerate

import Prelude as P

import System.Directory
import System.IO

import Acc.Lib

import Config

import Field (fdRes)
import Field.Hint (hintDescrFD)
import Field.Hint.Acc (buildPhaseSpace)

import Fluid

import Type

printer :: MonadIO m => Pipe a a m ()
printer =
  forM_ [(1::Int)..]
  (\i ->
      do
        await >>= yield
        if i `mod` 100 P.== 0 then liftIO $ print i else return ()
  )

main :: IO ()
main =
  do
    hSetBuffering stdout LineBuffering
    descr <- loadConfigFromArgs
    let
      mfp = descr ^. optOut
      V2 ydim xdim = descr ^. optHintDescr.hintDescrFD.fdRes
      dim = Z :. ydim :. xdim :: DIM2
      maxVecNorm = descr ^. optMaxVecNorm
      numFrames = descr ^. optNumFrames
      filePrefix = descr ^. optFilePrefix
    result <- buildPhaseSpace (descr ^. optHintDescr)
    case result of
      Left err -> print err
      Right v ->
        let
          !idf = makeDensity_hsv dim
          !maximumVecNorm' =
            run1 (A.maximum . A.flatten . A.map norm) v :: Array DIM0 Float
          maximumVecNorm = indexArray maximumVecNorm' Z
          multiplier = constant $ maxVecNorm / maximumVecNorm
          !ivf =
            run1
            (A.map
              (\vec ->
                 let (V2 y x) =
                       unlift $ vec ^* multiplier :: V2 (Exp Float)
                 in lift (y,x)
              ) . A.sum
            ) v

          outPipe =
              case mfp of
                Nothing -> openGLConsumerFlat dim
                Just fp ->
                  do
                    liftIO $ createDirectoryIfMissing True fp
                    forever (await >>= yield . flatToImage dim) >->
                      pngWriter 5 (fp P.++ "/" P.++ filePrefix)
        in
          do
            runSafeT $ runEffect $
              fluidProducer idf ivf >->
              printer >->
              maybe cat Pipes.take numFrames >->
              outPipe

fluidProducer
  :: Monad m
  => Array DIM2 (Float, RGB Float)
  -> Array DIM2 (Float,Float)
  -> Producer' (Array DIM1 Word8) m ()
fluidProducer idf ivf = f (idf,ivf)
  where
    step =
      run1
      (\arr ->
          let
            e = fluid 100 0.01 0 0 arr
            (df',vf') = unlift e :: (Acc (Field RGBDensity), Acc VelocityField)
            cf' = makePicture df'
            vf'' = A.zipWith (.+.) (use ivf) $ decayVelocity 0.9 vf'
          in
            lift (df', vf'', arrayToFlat cf')
      )

    f (df,vf) =
      do
        let (df',vf',cf') = step (df,idf,vf)
        yield cf'
        f (df',vf')
