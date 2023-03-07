{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Phases.Parser.Tests (testsParser) where

import           Data.VarName
import           Data.Primitive
import qualified Environment.MonadError as E
import           Phases.Parser

import           Control.Monad.Except
import           Test.HUnit

newtype TestEnvironment a = TestEnvironment (Either E.Error a)
  deriving (Functor, Applicative, Monad, MonadError E.Error, E.MonadError)

unwrap :: TestEnvironment a -> Either E.Error a
unwrap (TestEnvironment x) = x

mkTest :: String -> String -> Maybe Primitive-> Test
mkTest name arg expected = TestCase . assertEqual name expected . eitherToMaybe . unwrap $ parser arg
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _)  = Nothing

assignment :: String -> String -> Primitive
assignment = Assignment . either undefined id . varName

testsParser :: Test
testsParser = TestList [
  mkTest "Empty spaces"                     "  \t  \t "                    $ Just $ Command [],
  mkTest "Single command"                   "cmd"                          $ Just $ Command ["cmd"],
  mkTest "Single command with spaces"       "   cmd   "                    $ Just $ Command ["cmd"],
  mkTest "Other single command"             "qwer"                         $ Just $ Command $ "qwer" : [],
  mkTest "Other single command with spaces" "   qwer   "                   $ Just $ Command $ "qwer" : [],
  mkTest "Empty assignment"                 "x="                           $ Just $ assignment "x" "",
  mkTest "Empty assignment with spaces"     "   x=   "                     $ Just $ assignment "x" "",
  mkTest "Assignment"                       "x=1234"                       $ Just $ assignment "x" "1234",
  mkTest "Assignment with spaces"           "   x=1234   "                 $ Just $ assignment "x" "1234",
  mkTest "Assignment failure"               "x=1234 cmd"                   $ Nothing,
  mkTest "Command with arguments"           "cmd arg1 arg2 arg3"           $ Just $ Command $ "cmd" : ["arg1", "arg2", "arg3"],
  mkTest "Single quotes"                    "'x='"                         $ Just $ Command ["x="],
  mkTest "Double quotes"                    "\"x=\""                       $ Just $ Command ["x="],
  mkTest "Backslash ="                      "x\\="                         $ Just $ Command ["x="],
  mkTest "Quoted command"                   "c'm 'd a\"rg1' ar\"g2' arg3'" $ Just $ Command $ "cm d" : ["arg1' arg2 arg3"],
  mkTest "Quotes and spaces"                " cmd 'arg 1' \"arg 2\" "      $ Just $ Command $ "cmd" : ["arg 1", "arg 2"],
  mkTest "Terminated single quote"          "cmd 'arg1"                    $ Nothing,
  mkTest "Terminated double quote"          "cmd \"arg1 \\\""              $ Nothing,
  mkTest "Backslashes"                      "cmd arg\\ 1 arg\\ 2"          $ Just $ Command $ "cmd" : ["arg 1", "arg 2"],
  mkTest "Backslash space"                  "    \\    "                   $ Just $ Command $ " " : [],
  mkTest "Escaped quotes"                   "cmd \\\"arg1\\\"\\ arg2"      $ Just $ Command $ "cmd" : ["\"arg1\" arg2"],
  mkTest "Empty"                            ""                             $ Just $ Command []
  ]
