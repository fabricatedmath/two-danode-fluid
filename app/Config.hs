{-# LANGUAGE TemplateHaskell #-}

module Config
  ( loadConfigFromArgs, Descr(..)
  , optHintDescr, optOut, optMaxVecNorm, optFilePrefix
  )
where

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
  , _optOut :: Maybe FilePath
  , _optFilePrefix :: String
  , _optMaxVecNorm :: Float
  } deriving Show

makeLenses ''Descr

defaultDescr :: HintDescr Float -> Descr
defaultDescr hintDescr =
  Descr
  { _optHintDescr = hintDescr
  , _optOut = Nothing
  , _optMaxVecNorm = 0.02
  , _optFilePrefix = "v"
  }

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt Permute options args
    hintDescr <- loadHintDescrFromArgs "default" args
    _opts <- foldl (>>=) (pure $ defaultDescr hintDescr) actions
    return _opts

options :: [OptDescr (Descr -> IO Descr)]
options =
  [ Option "f" []
    (ReqArg
     (\arg -> (\opt -> pure $ optOut .~ Just arg $ opt))
     "Folder")
    "PNG file save folder"
  , Option "p" []
    (ReqArg
     (\arg -> (\opt -> pure $ optOut .~ Just arg $ opt))
     "String")
    "PNG file prefix"
  , Option "N" []
    (ReqArg
     (\arg -> (\opt -> pure $ optMaxVecNorm .~ read arg $ opt))
     "Float")
    "Max velocity norm"
  , Option "h" ["help"]
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
