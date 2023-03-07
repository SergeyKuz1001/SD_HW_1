{- |
В данном модуле объявлена монада @'MonadVarPathReader'@ для получения значения
переменной PATH.
-}
module Environment.MonadVarPathReader (
    MonadVarPathReader (..),
    varPathSeparator,
    parseVarPath,
  ) where

import Environment.FSPrimitive (AbsFilePath, absFilePath)
import Environment.MonadError (MonadError)

import Data.List (unfoldr)
import System.FilePath (pathSeparator)

-- | Монада для получения списка путей к директориям с исполняемыми файлами,
-- хранящегося в переменной PATH.
class Monad m => MonadVarPathReader m where
  getVarPath :: m [AbsFilePath]

-- | Функция для разбиения списка элементом по данному разделителю.
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep = unfoldr $ (\(p, ps) -> if null p then Nothing else Just (p, drop 1 ps)) . span (/= sep)

-- | Разделитель путей, записанных в переменной PATH.
--
-- @
-- (on Linux) varPathSeparator == \':\'
-- (on Windows) varPathSeparator == ';'
-- @
varPathSeparator :: Char
varPathSeparator = if pathSeparator == '/' then ':' else ';'

-- | Парсер исходного значения переменной PATH в список абсолютных путей.
parseVarPath :: MonadError m => String -> m [AbsFilePath]
parseVarPath = traverse absFilePath . splitBy varPathSeparator
