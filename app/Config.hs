{-# LANGUAGE TemplateHaskell #-}

module Config (loadConfigFromArgs, Descr(..), optHintDescr) where

import Control.Lens

import Data.Aeson (decode')
import qualified Data.ByteString.Lazy.Char8 as BS

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field
import Field.Hint

data Descr =
  Descr
  { _optHintDescr :: HintDescr Float
  } deriving Show

data Options =
  Options
  { _optFD :: FieldDescription Float
  , _optPolar :: Bool
  , _optR :: String
  , _optT :: String
  , _optF :: String
  , _optG :: String
  , _optReadJSON :: Maybe FilePath
  } deriving Show

makeLenses ''Descr
makeLenses ''Options

optionsToDescr :: Options -> Descr
optionsToDescr o =
  let
    hintDescr =
      let
        fieldStrings
          | _optPolar o =
            Polar
            { _rString = _optR o
            , _tString = _optT o
            }
          | otherwise =
            Cartesian
            { _fString = _optF o
            , _gString = _optG o
            }
      in
        HintDescr
        { _hintDescrFD = _optFD o
        , _hintDescrFS = fieldStrings
        }
  in
    Descr
    { _optHintDescr = hintDescr
    }

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    descr <-
      do
        let descr = optionsToDescr opts
        case _optReadJSON opts of
          Just path ->
            do
              mjson <- decode' <$> BS.readFile path
              case mjson of
                Nothing -> die "Failed to read JSON file"
                Just json -> pure $ (optHintDescr .~ json) descr
          Nothing -> pure descr
    pure descr

startOptions :: Options
startOptions =
  Options
  { _optFD = defaultFieldDescription
  , _optF = "y"
  , _optG = "-sin x"
  , _optPolar = False
  , _optR = "r*(1-r*r)"
  , _optT = "1"
  , _optReadJSON = Nothing
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
  , Option "" ["read-json"]
    (ReqArg
      (\arg opt -> pure $ optReadJSON .~ Just arg $ opt)
      "FILE")
    "Read JSON description of field equations"
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      )
    ) "Show help"
  ]
