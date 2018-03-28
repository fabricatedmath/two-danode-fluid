{-# LANGUAGE TemplateHaskell #-}

module Config (loadConfigFromArgs, Descr(..), optHintDescr) where

import Control.Lens

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field.Hint
import Field.Hint.Config

data Descr =
  Descr
  { _optHintDescr :: HintDescr Float
  } deriving Show

makeLenses ''Descr

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt Permute options args
    _opts <- foldl (>>=) (pure ()) actions
    fmap Descr $ loadHintDescrFromArgs "default" args

options :: [OptDescr (() -> IO ())]
options =
  [ Option "h" ["help"]
    (NoArg
      ((\_ -> do
          prg <- getProgName
          let stripType = map (fmap $ const ())
          hPutStrLn stderr $
            usageInfo prg
            ( stripType (hintDescrOptions :: [OptDescr (HintOption Double)]) ++
              stripType options
            )
          exitWith ExitSuccess
      ))
    ) "Show help"
  ]
