{- |
В данном модуле объявлен тип @'VarName'@ для хранения имени переменной и
вспомогательные функции для работы с ним.
-}
module Data.VarName (
    VarName (getVarName),
    varName,
    addChar,
  ) where

import Environment.MonadError (Error(..), MonadError, (?:))

-- | Имя переменной среды.
-- Тип необходим для поддержания инварианта того, что хранящаяся строка является
-- корректным именем переменной.
newtype VarName = VarName { getVarName :: String }
  deriving (Eq, Ord)

instance Show VarName where
  show (VarName name) = name

-- | Предикат принадлежности символа к классу символов, из которых может быть
-- образовано имя переменной.
isLCOU :: Char -> Bool
isLCOU c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])

-- | Функция для конструирования значения типа @'VarName'@. При конструировании
-- проверяет соблюдение инварианта, и в случае его нарушения вызывает ошибку.
varName :: MonadError m => String -> m VarName
varName str = do
  all isLCOU str ?: Error "ViolationOfInvariantError" (
    "\"" ++ str ++ "\" is not valid name of variable")
  return $ VarName str

addChar :: MonadError m => Char -> VarName -> m VarName
addChar c (VarName name) = do
  isLCOU c ?: Error "ViolationOfInvariantError" (
    "\"" ++ (c : name) ++ "\" is not valid name of variable")
  return . VarName $ c : name
