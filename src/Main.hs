{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Prelude hiding (FilePath)
import Shelly
import System.Console.CmdArgs
import Data.Text as T
default (T.Text)

data TestcaseFiles = TestcaseFiles {
    testcase_files :: [Text]
  } deriving (Data, Typeable, Show, Eq)

input_command = TestcaseFiles {
    testcase_files = def &= args &= typ "FILES"
  } &=
    verbosity &=
    help "Generates .t files from .t.in files" &=
    summary "gen-testcases (C) Justin (:flaviusb) Marsh" &=
    details ["gen-testcases generates .t files in the format I use from names and commands in .t.in files",""
            ,"To populate a t directory from the template files stored inside type:","  gen-testcases t/*.t.in"]

data TestInputFile = TestInputFile {
    file_name :: Text,
    file_couplets :: [(Text, Text)]
  } deriving (Data, Typeable, Show, Eq)

gettwo (a:b:lines) = (a, b):(gettwo lines)
gettwo _           = []

filesplitter filename = do
  contents <- readfile $ fromText filename 
  let split = gettwo $ T.lines contents
  let test_input_file = TestInputFile { file_name = filename, file_couplets = split }
  return test_input_file

main = shelly $ verbosely $ do
  input_struct <- liftIO $ cmdArgs input_command
  let files = testcase_files input_struct
  files_text <- mapM filesplitter files
  echo $ pack $ show files_text
