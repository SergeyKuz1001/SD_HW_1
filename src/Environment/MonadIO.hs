{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле объявлена монада @'MonadIO'@ для работы с потоками ввода/вывода.
-}
module Environment.MonadIO (
    MonadIO (..),
    putStrLn,
    print,
  ) where

import Data.VarName (VarName)
import Environment.FSPrimitive (AbsFilePath)
import Environment.MonadExit (ExitCode)

import qualified Data.ByteString.Char8 as ByteStr
import Prelude hiding (putStr, getLine, readFile, putStrLn, print)

-- | Монада для работы с потоками ввода/вывода.
class Monad m => MonadIO m where
  -- | Запись строки в поток вывода.
  putStr :: String -> m ()
  -- | Чтение строки из потока ввода. Возвращает Nothing если достигнут конец
  -- ввода (EOF).
  getLine :: m (Maybe String)
  -- | Чтение файла
  readFile :: AbsFilePath -> m String
  -- | Чтение файла бинарно.
  readFileFromBytes :: AbsFilePath -> m ByteStr.ByteString
  -- | Создание и исполнение исполняемого файла с данными аргументами и
  -- переменными. Возвращает код возврата процесса.
  createProcess :: AbsFilePath -> [String] -> [(VarName, String)] -> m ExitCode

-- | Запись строки в поток вывода и перевод строки.
putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

-- | Запись объекта в виде строки в поток вывода.
print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . show
