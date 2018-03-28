module Field.Hint.Acc where

import Data.Array.Accelerate as A hiding ((++))
import Data.Array.Accelerate.Array.Sugar (EltRepr)
import Data.Array.Accelerate.Linear
import Data.Array.Accelerate.IO.Data.Vector.Storable

import Language.Haskell.Interpreter

import Field
import Field.Hint

arrayToVector
  :: Shape sh
  => Array sh (V2 Float)
  -> Vectors (EltRepr (V2 Float))
arrayToVector = toVectors

vectorToArray
  :: Shape sh
  => sh
  -> Vectors (EltRepr (V2 Float))
  -> Array sh (V2 Float)
vectorToArray dim = fromVectors dim

buildPhaseSpace
  :: HintDescr Float
  -> IO (Either InterpreterError (Array DIM3 (V2 Float)))
buildPhaseSpace hintDescr =
  do
    let
      fd = _hintDescrFD hintDescr
      fieldS = _hintDescrFS hintDescr
      funcS = createFunction fieldS
      dim =
        let V2 y x = _fdRes fd
            aa' = _fdAA fd
        in Z :. y :. x :. aa'*aa' :: DIM3
    result <-
      runInterpreter $
      do
        let command =
              unwords
              [ "arrayToVector $ buildFieldAcc"
              , "(" ++ show fd ++ ")"
              , funcS
              ]
        setImports [ "Data.Vector.Storable"
                   , "Linear"
                   , "Prelude"
                   , "Field"
                   , "Field.Acc"
                   , "Field.Hint.Acc"
                   ]
        interpret command (as :: Vectors (EltRepr (V2 Float)))
    pure $ vectorToArray dim <$> result
