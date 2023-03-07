{- |
В данном модуле объявлена монада @'MonadVarsWriter'@ для записи значений
переменных.
-}
module Environment.MonadVarsWriter (
    module Environment.MonadVarPathWriter,
    module Environment.MonadVarPwdWriter,
    MonadVarsWriter (..),
    setVarPathDefault,
    setVarPwdDefault,
  ) where

import Data.VarName (VarName, varName)
import Environment.FSPrimitive (AbsFilePath(..))
import Environment.MonadError (MonadError)
import Environment.MonadVarPathWriter
import Environment.MonadVarPwdWriter

-- | Монада для записи значений переменных.
class (MonadVarPathWriter m, MonadVarPwdWriter m) => MonadVarsWriter m where
  -- | Запись значения переменной по её имени.
  setVar :: VarName -> String -> m ()

-- | Функция @'setVarPath'@, выраженная через функции монады
-- @'MonadVarsWriter'@.
setVarPathDefault :: (MonadVarsWriter m, MonadError m) => [AbsFilePath] -> m ()
setVarPathDefault absPaths = varName "PATH" >>= (flip setVar $ formatVarPath absPaths)

-- | Функция @'setVarPwd'@, выраженная через функции монады @'MonadVarsWriter'@.
setVarPwdDefault :: (MonadVarsWriter m, MonadError m) => AbsFilePath -> m ()
setVarPwdDefault absPath = varName "PWD" >>= (flip setVar $ asFilePath absPath)
