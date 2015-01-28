{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Prelude hiding (FilePath)
import Shelly
import System.Console.CmdArgs
import Text.Shakespeare.Text (lt)
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

coupletrunner couplet = do
  result <- cmd $ fromText $ snd couplet
  return (fst couplet, result)

runfile file = do
  new_couplets <- mapM coupletrunner $ file_couplets file
  return TestInputFile { file_name = file_name file, file_couplets = new_couplets }

main = shelly $ verbosely $ escaping False $ do
  input_struct <- liftIO $ cmdArgs input_command
  let files = testcase_files input_struct
  files_text <- mapM filesplitter files
  files_out_data <- mapM runfile files_text
  echo $ pack $ show files_text
  echo $ pack $ show files_out_data
