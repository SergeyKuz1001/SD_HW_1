{-# LANGUAGE LambdaCase #-}

{- |
Модуль для выполнения команд.
-}
module Phases.Executor (
    executor,
  ) where

import Data.ImprovedPrimitive
import Environment.MonadExit (MonadExit (exit))
import Environment.MonadIO as EnvIO
import Environment.MonadVarPwdReader (MonadVarPwdReader (getVarPwd))
import Environment.MonadVarsReader (MonadVarsReader (getVars))

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as ByteStr
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

type WcOutputArguments = (Int, Int, Int, Bool)

-- | Функция принимает, разбирает и исполняет распарсшенный примитив.
executor :: (MonadIO m, MonadExit m, MonadVarsReader m) => Primitive -> m ()
executor = \case
  Command typeCmd -> case typeCmd of
    Special special -> executeSpecial special
    Common common -> case common of
      Internal internal -> executeInternal internal
      External external -> executeExternal external
  EmptyCommand -> return ()

-- | Функция для исполнения специальных команд.
executeSpecial :: (MonadExit m) => Special -> m ()
executeSpecial = \case
  Exit mb -> exit (fromMaybe 0 mb)

-- | Функция для исполнения внутренних команд.
executeInternal :: (MonadIO m, MonadVarPwdReader m) => Internal -> m ()
executeInternal = \case
  Cat filePath -> do
    file <- EnvIO.readFile filePath
    EnvIO.putStrLn file
  Echo ls -> do
    EnvIO.putStrLn $ drop 1 $ concatMap (' ' :) ls
  Wc filePath -> do
    file <- EnvIO.readFileFromBytes filePath
    let (countLines, countWords, bytes, isPrevSpace) = ByteStr.foldl' wcgo (0, 0, 0, False) file
    let newCountWords = countWords + bool 1 0 isPrevSpace
    EnvIO.putStrLn $ show countLines ++ ' ' : show newCountWords ++ ' ' : show bytes
  Pwd -> do
    pwd <- getVarPwd
    EnvIO.print pwd

  where
    wcgo :: WcOutputArguments -> Char -> WcOutputArguments
    wcgo (countLines, countWords, bytes, isPrevSpace) c =
      (countLines + checkOnLine, countWords + checkOnWord, bytes + 1, isSpace c)
      where
        checkOnLine = bool 0 1 $ c == '\n'
        checkOnWord = bool 0 1 $ not isPrevSpace && isSpace c

-- | Функция для исполнения внешних команд.
executeExternal :: (MonadIO m, MonadVarsReader m) => External -> m ()
executeExternal = \case
  Arguments pathToCmd args -> do
    vars <- getVars
    _ <- createProcess pathToCmd args vars
    return ()
