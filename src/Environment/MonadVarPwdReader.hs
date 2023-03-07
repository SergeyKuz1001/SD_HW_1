{- |
В данном модуле объявлена монада @'MonadVarPwdReader'@ для получения значения
переменной PWD.
-}
module Environment.MonadVarPwdReader (
    MonadVarPwdReader (..),
  ) where

import Environment.FSPrimitive (AbsFilePath)

-- | Монада для получения значения переменной PWD (то есть абсолютного пути до
-- текущей директории).
class Monad m => MonadVarPwdReader m where
  getVarPwd :: m AbsFilePath
