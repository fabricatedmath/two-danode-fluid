{-
BSD 3-Clause License

Copyright (c) 2018, Charles Durham, [2007..2017] The Accelerate Team
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

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
