{-# LANGUAGE FlexibleInstances #-}
--
-- Types used throughout the simulation
--

module Type where

import Data.Word
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Colour.Names
import Data.Array.Accelerate.Data.Colour.RGB

type Timestep           = Float
type Viscosity          = Float
type Diffusion          = Float
type Index              = DIM2
type Density            = Float
type Velocity           = (Float, Float)

type RGBDensity         = (Float, RGB Float)

type Field a            = Array DIM2 a
type DensityField       = Field Density
type VelocityField      = Field Velocity

type RGBA32             = Word32
type Image a            = Array DIM2 a

infixl 6 .+.
infixl 6 .-.
infixl 7 .*.
infixl 7 ./.

class Elt e => FieldElt e where
  zero  :: Exp e
  (.+.) :: Exp e -> Exp e -> Exp e
  (.-.) :: Exp e -> Exp e -> Exp e
  (.*.) :: Exp Float -> Exp e -> Exp e
  (./.) :: Exp e -> Exp Float -> Exp e

instance FieldElt Density where
  zero  = 0
  (.+.) = (+)
  (.-.) = (-)
  (.*.) = (*)
  (./.) = (/)

instance FieldElt RGBDensity where
  zero = lift (0 :: Exp Float,white :: RGB Float)
  (.+.) x1 x2 =
    let
      (a1,c1) = unlift x1
      (a2,c2) = unlift x2
    in
      lift (a1 + a2,blend a1 a2 c1 c2)
  (.-.) = undefined
  (.*.) m x =
    let
      (a,c) = unlift x :: (Exp Float, Exp (RGB Float))
    in
      lift (m*a,c)
  (./.) x d =
    let
      (a,c) = unlift x :: (Exp Float, Exp (RGB Float))
    in
      lift (a/d,c) :: Exp RGBDensity

instance FieldElt Velocity where
  zero  = constant (0, 0)
  (.+.) = app2 (+)
  (.-.) = app2 (-)
  c  .*. xy = let (x,y) = unlift xy in lift (c*x, c*y)
  xy ./. c  = let (x,y) = unlift xy in lift (x/c, y/c)

app2 :: Elt e => (Exp e -> Exp e -> Exp e) -> Exp (e,e) -> Exp (e,e) -> Exp (e,e)
app2 f xu yv = let (x,u) = unlift xu
                   (y,v) = unlift yv
               in  lift (f x y, f u v)
