{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{- |
В данном модуле объявлен тип @'Environment'@ как главный контекст, в котором
будут происходить все основные действия данной программы.
-}
module Environment (
    module Environment.FSPrimitive,
    module Environment.MonadError,
    module Environment.MonadExit,
    module Environment.MonadFS,
    module Environment.MonadIO,
    module Environment.MonadVarPathReader,
    module Environment.MonadVarPwdReader,
    module Environment.MonadVarsReader,
    Environment,
    runEnvironment,
  ) where

import Data.VarName (VarName(..), varName)
import Environment.FSPrimitive
import Environment.MonadError
import Environment.MonadExit
import Environment.MonadFS
import Environment.MonadIO
import Environment.MonadVarPathReader
import Environment.MonadVarPwdReader
import Environment.MonadVarsReader

import qualified Control.Monad.Except as ME
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Prelude hiding (putStr, putStrLn, getLine, readFile)
import qualified Prelude as P
import qualified System.Directory as D
import System.Environment (lookupEnv, getEnvironment)
import System.Exit (exitWith, ExitCode(..))
import System.IO (isEOF)
import qualified System.Process as PRC

-- | Главный контекст для вычислений в программе.
newtype Environment a = Environment (ME.ExceptT Error IO a)
  deriving (Functor, Applicative, Monad, ME.MonadError Error, MonadError, MIO.MonadIO)

-- | Функция-обёртка действий из монады @'IO'@ в монаду @'Environment'@.
toEnv :: IO a -> Environment a
toEnv = Environment . MIO.liftIO

instance MonadIO Environment where
  putStr  = toEnv . P.putStr
  getLine = toEnv $ do
    eof <- isEOF
    if eof
      then return Nothing
      else Just <$> P.getLine
  readFile absPath = toEnv $ P.readFile $ asFilePath absPath
  readFileFromBytes absPath = toEnv $ BS.readFile $ asFilePath absPath
  createProcess absPath args vars = toEnv $ do
    let vars' = map (\(name, value) -> (getVarName name, value)) vars
    procHndl <- PRC.runProcess (asFilePath absPath) args Nothing (Just vars') Nothing Nothing Nothing
    exitCode <- PRC.waitForProcess procHndl
    return $ case exitCode of
      ExitSuccess   -> 0
      ExitFailure x -> x

instance MonadVarPathReader Environment where
  getVarPath = getVarPathDefault

instance MonadVarPwdReader Environment where
  getVarPwd = toEnv D.getCurrentDirectory >>= absFilePath

instance MonadVarsReader Environment where
  getVar name = do
    mValue <- toEnv . lookupEnv $ getVarName name
    return $ fromMaybe "" mValue
  getVars = do
    vars <- toEnv getEnvironment
    traverse (\(name, value) -> (, value) <$> varName name) vars

instance MonadFS Environment where
  findFileByAbsPath absPath = toEnv $ do
    let path = asFilePath absPath
    exists <- D.doesFileExist path
    if not exists
      then return Nothing
      else do
        perms <- D.getPermissions path
        let perms' = (Permissions <$> D.readable <*> D.writable <*> D.executable) perms
        return . Just $ File absPath perms'

instance MonadExit Environment where
  exit code = toEnv . exitWith $
    if code == 0 then ExitSuccess else ExitFailure code

-- | Функция для запуска вычислений.
runEnvironment :: Environment a -> IO a
runEnvironment (Environment m) = do
  eRes <- ME.runExceptT m
  case eRes of
    Right res -> return res
    Left err  -> fail $ "UnexpectedError: " ++ show err
