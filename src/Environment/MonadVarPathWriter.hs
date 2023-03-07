{- |
В данном модуле объявлена монада @'MonadVarPathWriter'@ для записи значения
переменной PATH.
-}
module Environment.MonadVarPathWriter (
    MonadVarPathWriter (..),
    formatVarPath,
  ) where

import Environment.FSPrimitive (AbsFilePath(asFilePath))
import Environment.MonadVarPathReader (varPathSeparator)

-- | Монада для записи списка абсолютных путей в переменную PATH.
class Monad m => MonadVarPathWriter m where
  setVarPath :: [AbsFilePath] -> m ()

-- | Функция преобразования списка путей в строку, которая будет являться
-- корректным значением переменной PATH.
formatVarPath :: [AbsFilePath] -> String
formatVarPath [] = ""
formatVarPath afps = foldr1 (\x y -> x ++ pure varPathSeparator ++ y) $ map asFilePath afps
