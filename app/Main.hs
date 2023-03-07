{-# LANGUAGE NoImplicitPrelude #-}

{- |
В данном модуле определена главная вызываемая функция @'main'@, а также её более
"чистый" аналог @'main\''@.
-}
module Main (
    main,
  ) where

import Environment
import Phases

import Control.Monad (forever)
import Prelude hiding (print)
import System.IO (BufferMode(..), hSetBuffering, stdin, stdout, stderr)

-- | Точка входа в программу. Здесь настраивается буфферизация стандартных
-- потоков и запускается более абстрактная функция @'main\''@ в монаде
-- @'Environment'@.
main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  runEnvironment main'

-- | Более абстрактный аналог @'main'@, главный цикл программы. Каждая итерация
-- цикла представляет собой последовательность обработчиков @'stringReader'@ →
-- @'parser'@ → @'analyzer'@ → @'executor'@. При возникновении ошибки на любом
-- из этапов происходит печать ошибки и начало новой итерации. Цикл бесконечен,
-- для выхода из него (и из программы в целом) существует функция @'exit'@ в
-- классе @'MonadExit'@.
main' :: (MonadError m, MonadIO m, MonadFS m, MonadVarsReader m, MonadExit m) => m ()
main' = forever $ (
      stringReader >>= parser >>= analyzer >>= executor
    ) `catchError` print
