{- |
В данном модуле объявлена монада @'MonadVarsReader'@ для получения значений
переменных.
-}
module Environment.MonadVarsReader (
    module Environment.MonadVarPathReader,
    module Environment.MonadVarPwdReader,
    MonadVarsReader (..),
    getVarPathDefault,
    getVarPwdDefault,
  ) where

import Data.VarName (VarName, varName)
import Environment.FSPrimitive (AbsFilePath, absFilePath)
import Environment.MonadError (MonadError)
import Environment.MonadVarPathReader
import Environment.MonadVarPwdReader

-- | Монада для получения значений переменных.
class (MonadVarPathReader m, MonadVarPwdReader m) => MonadVarsReader m where
  -- | Получение значения переменной по имени. Если переменной с таким именем
  -- нет, то необходимо вернуть пустую строку.
  getVar :: VarName -> m String
  -- | Получение значений всех объявленных переменных в виде списка пар (имя,
  -- значение).
  getVars :: m [(VarName, String)]

-- | Функция @'getVarPath'@, выраженная через функции монады
-- @'MonadVarsReader'@.
getVarPathDefault :: (MonadVarsReader m, MonadError m) => m [AbsFilePath]
getVarPathDefault = varName "PATH" >>= getVar >>= parseVarPath

-- | Функция @'getVarPwd'@, выраженная через функции монады @'MonadVarsReader'@.
getVarPwdDefault :: (MonadVarsReader m, MonadError m) => m AbsFilePath
getVarPwdDefault = varName "PWD" >>= getVar >>= absFilePath
