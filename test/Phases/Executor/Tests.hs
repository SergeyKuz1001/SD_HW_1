{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Phases.Executor.Tests (
    testsExecutor
) where

import Data.ImprovedPrimitive
import Data.VarName (VarName(..), varName)
import Environment.FSPrimitive (AbsFilePath(..), absFilePath)
import Environment.MonadExit (ExitCode, MonadExit (exit))
import Environment.MonadVarsReader
import Environment.MonadIO as EnvIO ( MonadIO(..) )
import Phases.Executor (executor)

import Control.Monad.State hiding (MonadIO)
import Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import Test.HUnit (Test(TestList, TestCase), assertEqual)

type TestFileInfo = (AbsFilePath, String)

data IOState = IOState
  { stdOut :: String,
    externalCommands :: [(AbsFilePath, [String], [(VarName, String)])],
    pwd :: AbsFilePath,
    vars :: Map.Map VarName String,
    exitCode :: Maybe ExitCode
  }
  deriving (Eq, Show)

newtype TestEnvironment a = TestEnvironment { runTestEnvironment :: State IOState a }
  deriving (Functor, Applicative, Monad)

instance MonadIO TestEnvironment where
  putStr str = TestEnvironment $ modify (\st -> st { stdOut = str })
  readFile absPath = TestEnvironment . return $ files Map.! absPath
  readFileFromBytes filePath = EnvIO.readFile filePath >>= (TestEnvironment . return . pack)
  createProcess absPath args vars = TestEnvironment (modify (\st -> st { externalCommands = (absPath, args, vars) : externalCommands st })) >> return 0
  getLine = undefined -- Not used

instance MonadVarPwdReader TestEnvironment where
  getVarPwd = TestEnvironment $ gets pwd

instance MonadVarPathReader TestEnvironment where
  getVarPath = undefined -- Not used

instance MonadVarsReader TestEnvironment where
  getVar str = TestEnvironment $ gets ((Map.! str) . vars)
  getVars = TestEnvironment $ gets (Map.toList . vars)

instance MonadExit TestEnvironment where
  exit code = TestEnvironment $ modify (\st -> st { exitCode = Just code })

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = foldr (\z -> if x == z then (y:) else (z:)) []

absFilePath' :: String -> AbsFilePath
absFilePath' path = case absFilePath path of
  Left _ -> either undefined id . absFilePath $ "C:" ++ replace '/' '\\' path
  Right absPath -> absPath

varName' :: String -> VarName
varName' = either undefined id . varName

fileTestExample :: TestFileInfo
fileTestExample = (absFilePath' "/Example.txt", "Some example text")
fileTestMSE :: TestFileInfo
fileTestMSE = (absFilePath' "/MSE.txt", "I love MSE!")

files :: Map.Map AbsFilePath String
files = Map.fromList
  [fileTestExample, fileTestMSE]

checkState :: String -> IOState -> IOState -> Primitive -> Test
checkState textError excepted actual prim = TestCase $ assertEqual textError excepted $ execState (runTestEnvironment (executor prim)) actual

emptyState :: IOState
emptyState = IOState {
  stdOut = "",
  externalCommands = [],
  pwd = absFilePath' "/",
  vars = Map.empty,
  exitCode = Nothing
}

testsExecutor :: Test
testsExecutor = TestList [
  checkState "empty command" emptyState emptyState EmptyCommand,  
  checkState "exit - check exit code" (emptyState { exitCode = Just 2 }) emptyState $ Command $ Special $ Exit $ Just 2,

  let (fileAbsPath, fileText) = fileTestMSE
    in checkState "cat -- MSE file" (emptyState { stdOut = fileText ++ "\n" }) emptyState $ Command $ Common $ Internal $ Cat fileAbsPath,
  let (fileAbsPath, fileText) = fileTestExample
    in checkState "cat -- Example file" (emptyState { stdOut = fileText ++ "\n" }) emptyState $ Command $ Common $ Internal $ Cat fileAbsPath,

  let output = "Hello"
    in checkState "echo -- One word" (emptyState { stdOut = output ++ "\n" }) emptyState $ Command $ Common $ Internal $ Echo $ words output,
  let output = "It's a new day!"
    in checkState "echo -- Few word" (emptyState { stdOut = output ++ "\n" }) emptyState $ Command $ Common $ Internal $ Echo $ words output,

  let (fileAbsPath, _) = fileTestMSE
    in checkState "wc -- MSE file" (emptyState { stdOut = "0 3 11\n"}) emptyState $ Command $ Common $ Internal $ Wc fileAbsPath,
  let (fileAbsPath, _) = fileTestExample
    in checkState "wc -- example file" (emptyState { stdOut = "0 3 17\n"}) emptyState $ Command $ Common $ Internal $ Wc fileAbsPath,

  let absPwd = absFilePath' "/home/"
    in checkState "pwd" (emptyState { stdOut = show absPwd ++ "\n", pwd = absPwd }) (emptyState { pwd = absPwd }) $ Command $ Common $ Internal Pwd,

  let cmd@(name, args, vars) = (absFilePath' "/usr/bin/ls", ["~/example/"], [(varName' "example", "test")])
    in let varsMap = Map.fromList vars
      in checkState "external 'ls'" (emptyState { externalCommands = [cmd], vars = varsMap }) (emptyState { vars = varsMap }) $ Command $ Common $ External $ Arguments name args
  ]
