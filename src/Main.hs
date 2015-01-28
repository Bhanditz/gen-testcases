{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Prelude hiding (FilePath)
import Shelly
import System.Console.CmdArgs
import Data.Text as T
default (T.Text)

data Inputs = Inputs {
    files :: [Text]
  } deriving (Data,Typeable,Show,Eq)

inputs = Inputs {
    files = def &= args &= typ "FILES"
  } &=
    verbosity &=
    help "Generates .t files from .t.in files" &=
    summary "gen-testcases (C) Justin (:flaviusb) Marsh" &=
    details ["gen-testcases generates .t files in the format I use from names and commands in .t.in files",""
            ,"To populate a t directory from the template files stored inside type:","  gen-testcases t/*.t.in"]

main = shelly $ verbosely $ do
  files <- liftIO $ cmdArgs inputs
  echo $ pack $ show files
