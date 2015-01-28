{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Prelude hiding (FilePath)
import Shelly
import System.Console.CmdArgs
import System.FilePath (dropExtension)
import Text.Shakespeare.Text (st)
import Data.Monoid (mconcat)
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

data TestOutputFile = TestOutputFile {
    file_name_out :: Text,
    file_couplets_out :: [(Text, Text, Text)]
  } deriving (Data, Typeable, Show, Eq)

data OutFile = OutFile {
    file_write_name :: Text,
    file_contents :: Text
  } deriving (Data, Typeable, Show, Eq)

make_test :: (Text, Text, Text) -> Text
make_test (name, command, expected_value) = [st|

name "#{name}"
first=`#{command}`
first_expect='#{expected_value}'
expect_eq "$first" "$first_expect"

|]

put_through_templates :: TestOutputFile -> OutFile
put_through_templates file =
  let rest = mconcat $ Prelude.map make_test $ file_couplets_out file
      top = [st|#!/bin/bash

. $(dirname $0)/test.sh

plan #{show $ Prelude.length $ file_couplets_out file}

|] in
  OutFile {file_write_name = file_name_out file, file_contents = mconcat [top, rest]}
  
write_file file = do
  writefile (fromText $ file_write_name file) (file_contents file)

gettwo (a:b:lines) = (a, b):(gettwo lines)
gettwo _           = []

filesplitter filename = do
  contents <- readfile $ fromText filename 
  let split = gettwo $ T.lines contents
  let test_input_file = TestInputFile { file_name = filename, file_couplets = split }
  return test_input_file

coupletrunner couplet = do
  result <- cmd $ fromText $ snd couplet
  return (fst couplet, snd couplet, result)

runfile file = do
  new_couplets <- mapM coupletrunner $ file_couplets file
  return TestOutputFile { file_name_out = pack $ dropExtension $ unpack $ file_name file, file_couplets_out = new_couplets }

main = shelly $ escaping False $ do
  input_struct <- liftIO $ cmdArgs input_command
  let files = Prelude.filter (\file -> hasExt "in" $ fromText file) $ testcase_files input_struct
  files_text <- mapM filesplitter files
  files_out_data <- mapM runfile files_text
  let each_file_out = Prelude.map put_through_templates files_out_data
  --echo $ pack $ show files_text
  --echo $ pack $ show files_out_data
  --echo $ pack $ show each_file_out
  mapM_ write_file each_file_out
