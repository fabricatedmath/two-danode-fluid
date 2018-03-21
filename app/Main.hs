{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forever)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Linear

import Data.Array.Accelerate.Data.Colour.RGB            as RGB
import Data.Array.Accelerate.Data.Colour.HSV            as HSV

import Data.Function (on)

import Language.Haskell.Interpreter hiding (lift)

import Pipes hiding (lift)
import Pipes.Safe
import Pipes.Graphics.Accelerate

import Prelude as P

import Field
import Field.Hint

type Field a = Array DIM2 a

defaultFD :: P.Num a => DIM2 -> FieldDescription a
defaultFD (Z :. ydim :. xdim) = FromCenter (V2 ydim xdim) (V2 0 0) (2) (1)

defaultFS :: FieldStrings
defaultFS = Cartesian "sin(13*x)*cos(10*y*x)" "sin(5*x)*cos (10*y*x)"

main :: IO ()
main =
  do
    let
      ydim = 1000
      xdim = 1000
      dim = Z :. ydim :. xdim :: DIM2
      density = makeDensity_hsv dim
      !image = run1 (A.map packRGBTupA . makePicture) density
    space <- buildPhaseSpace (defaultFD dim) defaultFS
    --print space
    runSafeT $ runEffect $ forever (yield image) >-> openGLConsumer' dim
{-
    putStrLn "Test interpreter/hint"
    r <- runInterpreter $ do
      setImports ["Prelude"]
      interpret "1+2" (as :: Int)
    print r
-}

makeDensity_hsv :: DIM2 -> Array DIM2 (Float, RGB Float)
makeDensity_hsv dim@(Z :. height :. width) =
  let
    xdim = P.fromIntegral width
    ydim = P.fromIntegral height
    ybound = 4
    xbound = 4

    wrapHsv :: DIM2 -> (Float, HSV Float)
    wrapHsv (Z :. yc' :. xc') =
      let
        yc = (P.fromIntegral yc' / ydim * 2 - 1) * xbound
        xc = (P.fromIntegral xc' / xdim * 2 - 1) * ybound
        hypoLen =
          let f x l = l*l*(x*x +1)
          in sqrt $ P.min (f (xc/yc) xbound) (f (yc/xc) ybound)
        dist = sqrt $ xc*xc + yc*yc
        angleNorm' = acos (xc / dist) / (2*pi) * 360
        angleNorm = angleNorm' + isObtuse*2*(180 - angleNorm')
        ratio = dist/hypoLen
        isObtuse = P.max 0 $ negate $ signum yc
      in
        (1,HSV angleNorm ratio 1)
  in
    run1 (A.map (\v -> lift (A.fst v,toRGB $ A.snd v)))
    $ fromFunction dim wrapHsv

makePicture :: Acc (Field (Float,RGB Float)) -> Acc (Field (Word8,Word8,Word8))
makePicture df = A.map (rgbToTuple . f) df
  where
    maxV = the $ foldAll A.max 0 $ A.map (A.fst) df
    f e =
      let (d,a) = unlift e :: (Exp Float, Exp (RGB Float))
          (HSV h s v) = unlift $ HSV.fromRGB a :: HSV (Exp Float)
      in
        HSV.toRGB $ lift $ HSV h s (d / maxV)

rgbToTuple :: Exp RGB.Colour -> Exp (Word8,Word8,Word8)
rgbToTuple ec =
  let
    (RGB.RGB r g b) = unlift ec
    r' = word8OfFloat r
    g' = word8OfFloat g
    b' = word8OfFloat b
  in
    lift (r',g',b')

word8OfFloat :: Exp Float -> Exp Word8
word8OfFloat x = A.truncate (x * 255)
