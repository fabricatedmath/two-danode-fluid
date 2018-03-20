module Main where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX
import Language.Haskell.Interpreter

import Lib

main :: IO ()
main =
  do
    putStrLn "Test accelerate"
    print $ run1 (A.map (+1)) $ A.fromList (Z :. 10 :: DIM1) ([0..] :: [Float])

    putStrLn "Test interpreter/hint"
    r <- runInterpreter $ do
      setImports ["Prelude"]
      interpret "1+2" (as :: Int)
    print r
