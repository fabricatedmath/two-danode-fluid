{-# LANGUAGE FlexibleContexts #-}

module Acc.Lib where

import Data.Array.Accelerate as A

import Data.Array.Accelerate.Data.Colour.RGB            as RGB
import Data.Array.Accelerate.Data.Colour.HSV            as HSV

import Data.Array.Accelerate.LLVM.PTX

import Data.Array.Accelerate.Linear

import qualified Linear as L

import Prelude as P

import Type

decayDensity :: Exp Float -> Acc (Field RGBDensity) -> Acc (Field RGBDensity)
decayDensity rate arr =
  let
    f e =
      let
        (d,a) = unlift e :: (Exp Float, Exp (RGB Float))
      in
        lift (d * rate, a)
  in
    A.map f arr

decayVelocity :: Exp Float -> Acc VelocityField -> Acc VelocityField
decayVelocity rate arr =
  let
    f e =
      let
        (x,y) = unlift e :: (Exp Float, Exp Float)
      in lift (x*rate,y*rate)
  in
    A.map f arr

makeDensity_rgb :: DIM2 -> Array DIM2 (Float, RGB Float)
makeDensity_rgb dim@(Z :. height :. width) =
  let
    xdim = P.fromIntegral width
    ydim = P.fromIntegral height
    --ybound = 4
    --xbound = 4

    wrapHsv :: DIM2 -> (Float, RGB Float)
    wrapHsv (Z :. yc' :. xc') =
      let
        yc = (P.fromIntegral yc' / ydim)
        xc = (P.fromIntegral xc' / xdim)
      in
        (1,RGB.RGB yc xc 1)
  in
    fromFunction dim wrapHsv

--TODO: add better method for positioning "camera"
--TODO: deal with xc,yc == 0
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
        if L.nearZero yc P.&& L.nearZero xc
        then (0,HSV 0 0 0) else (1,HSV angleNorm ratio 1)
  in
    run1 (A.map (\v -> lift (A.fst v,toRGB $ A.snd v)))
    $ fromFunction dim wrapHsv

makeDensity_test :: DIM2 -> Array DIM2 (V2 Float)
makeDensity_test dim@(Z :. height :. width) =
  let
    xdim = P.fromIntegral width
    ydim = P.fromIntegral height
    ybound = 4
    xbound = 4

    wrapHsv :: DIM2 -> V2 Float
    wrapHsv (Z :. yc' :. xc') =
      let
        yc = (P.fromIntegral yc' / ydim * 2 - 1) * xbound
        xc = (P.fromIntegral xc' / xdim * 2 - 1) * ybound
      in V2 yc xc
  in
    fromFunction dim wrapHsv

makePicture :: Acc (Field (Float,RGB Float)) -> Acc (Field (Word8,Word8,Word8))
makePicture df = A.map (rgbToTuple . f) df
  where
    maxV = the $ foldAll A.max 0 $ A.map (A.fst) df
    f e =
      let (d,a) = unlift e :: (Exp Float, Exp (RGB Float))
          (HSV h s _v) = unlift $ HSV.fromRGB a :: HSV (Exp Float)
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
