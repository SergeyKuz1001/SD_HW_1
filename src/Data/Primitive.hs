{- |
В данном модуле объявлены основные примитивы, в которые транслируется
пользовательский запрос после его парсинга.
-}
module Data.Primitive (
    Primitive (..),
  ) where

import Data.VarName

-- | Примитив, полученный после парсинга пользовательского запроса.
data Primitive
  = Command [String]
  | Assignment VarName String
  deriving (Eq, Show)
