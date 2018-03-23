{-# LANGUAGE TemplateHaskell #-}

module Config (loadConfigFromArgs, Descr(..), descrFD, descrFS) where

import Control.Lens

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field
import Field.Hint

data Descr =
  Descr
  { _descrFD :: FieldDescription Float
  , _descrFS :: FieldStrings
  } deriving Show

data Options =
  Options
  { _optFD :: FieldDescription Float
  , _optPolar :: Bool
  , _optR :: String
  , _optT :: String
  , _optF :: String
  , _optG :: String
  } deriving Show

makeLenses ''Descr
makeLenses ''Options

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let
      fieldStrings
        | _optPolar opts =
             Polar
             { _rString = _optR opts
             , _tString = _optT opts
             }
        | otherwise =
            Cartesian
            { _fString = _optF opts
            , _gString = _optG opts
            }
    return $
      Descr
      { _descrFD = _optFD opts
      , _descrFS = fieldStrings
      }

startOptions :: Options
startOptions =
  Options
  { _optFD = defaultFieldDescription
  , _optF = "y"
  , _optG = "-sin x"
  , _optPolar = False
  , _optR = "r*(1-r*r)"
  , _optT = "1"
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "F" []
    (ReqArg
     (\arg opt -> pure $ optF .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for 'x dot', F (x,y) = _"
                ,"Default: " ++ show (_optF startOptions)]
  , Option "G" []
    (ReqArg
     (\arg opt -> pure $ optG .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for 'y dot', G (x,y) = _"
                ,"Default: " ++ show (_optG startOptions)]
  , Option "P" []
    (NoArg
     (\opt -> pure $ optPolar .~ True $ opt))
    $ unlines $ ["Set to use polar coordinates in terms of r and theta"
                ,"as r-dot and theta-dot"]
  , Option "R" []
    (ReqArg
     (\arg opt -> pure $ optR .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for polar 'r dot', R (r,theta) = _"
                ,"Default: " ++ show (_optR startOptions)]
  , Option "T" []
    (ReqArg
     (\arg opt -> pure $ optT .~ arg $ opt)
      "String")
    $ unlines $ ["Function String for polar 'theta dot', T (r,theta) = _"
                ,"Default: " ++ show (_optT startOptions)]
  , Option "r" ["res"]
    (ReqArg
     (\arg opt -> pure $ optFD.fdRes .~ read arg ^. tupleToV2 $ opt)
      "(Int,Int)")
    $ unlines $
    ["Resolution of output image, needs quotes in cmd line"
    ,"Default: " ++ show (startOptions ^. optFD.fdRes.v2ToTuple.to show)]
  , Option "c" ["center"]
    (ReqArg
     (\arg opt -> pure $ optFD.fdCenter .~ read arg ^. tupleToV2 $ opt)
      "(Double,Double)")
    $ unlines $
    ["Center of field View, needs quotes in cmd line"
    ,"Default: " ++ show (startOptions ^. optFD.fdCenter.v2ToTuple.to show)]
  , Option "H" ["height"]
    (ReqArg
     (\arg opt -> pure $ optFD.fdHeight .~ read arg $ opt)
      "Double")
    $ unlines $ ["Height of field view"
                ,"Default: " ++ show (startOptions ^. optFD.fdHeight)]
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      )
    ) "Show help"
  ]
