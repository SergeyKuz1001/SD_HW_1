{- |
В данном модуле объявлена монада @'MonadVarPwdReader'@ для записи значения
переменной PWD.
-}
module Environment.MonadVarPwdWriter (
    MonadVarPwdWriter (..),
  ) where

import Environment.FSPrimitive (AbsFilePath)

-- | Монада для записи абсолютного пути до директории в переменную PWD.
class Monad m => MonadVarPwdWriter m where
  setVarPwd :: AbsFilePath -> m ()
