{-# LANGUAGE NoImplicitPrelude #-}

{- |
Модуль предназначен для чтения пользовательского ввода (запроса).
-}
module Phases.StringReader (
    stringReader,
  ) where

import Environment.MonadIO

import Prelude hiding (putStr, putStrLn, getLine)

-- | Чтение пользовательского запроса.
stringReader :: MonadIO m => m String
stringReader = do
  putStr "$ "
  mStr <- getLine
  case mStr of
    Nothing -> do
      putStrLn "exit"
      return "exit"
    Just str ->
      return str
