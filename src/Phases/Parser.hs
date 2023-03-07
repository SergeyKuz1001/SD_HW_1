{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Модуль для парсинга пользовательского запроса.
-}
module Phases.Parser ( parser ) where

import           Data.VarName            (varName, addChar)
import           Data.Primitive          (Primitive (..))
import           Environment.MonadError  (Error (..), MonadError, throwError)

import           Data.Bifunctor          (first)
import           Prelude

error' :: String -> Error
error' = Error "ParsingError"

-- | Функция для разбора строки после подстановки переменных.
-- Выделяет либо команду с аргументами,
-- либо пустую команду (пустая строка или строка только из пробелов),
-- либо присваивание переменной.
-- Кидает исключение, если строка имеет некорректный синтаксис.
parser :: MonadError m => String -> m Primitive
parser s = skipSpaces s >>= firstWord

-- | Шаг парсера, пропускающий пробелы и табуляции до первого значащего символа.
skipSpaces :: Applicative f => String -> f String
skipSpaces (' '  : cs) = skipSpaces cs
skipSpaces ('\t' : cs) = skipSpaces cs
skipSpaces s           = pure s

-- | Определение первого слова.
-- Здесь происходит выбор типа строки: пустая строка, команда, присваивание.
firstWord :: MonadError m => String -> m Primitive
firstWord     ""                 = pure $ Command []
firstWord   ( '\\' : '\\' : cs ) = firstWord cs >>= prependChar '\\'
firstWord   ( '\\' : '\"' : cs ) = firstWord cs >>= prependChar '\"'
firstWord   ( '\\' : '\'' : cs ) = firstWord cs >>= prependChar '\''
firstWord   ( '\\' : ' '  : cs ) = Command . filter (not . null) . headMap (' ' :) <$> splitBySpaces cs
firstWord   ( '\\' : '='  : cs ) = Command . filter (not . null) . headMap ('=' :) <$> splitBySpaces cs
firstWord   (        ' '  : cs ) = Command . ("" :) . filter (not . null)          <$> splitBySpaces cs
firstWord   (        '\t' : cs ) = firstWord (' ' : cs)
firstWord s@(        '\'' : _  ) = Command . filter (not . null) <$> splitBySpaces s
firstWord s@(        '\"' : _  ) = Command . filter (not . null) <$> splitBySpaces s
firstWord   (        '='  : cs ) = Assignment <$> varName "" <*> parseValue cs
firstWord   (        c    : cs ) = firstWord cs >>= prependChar c

-- | Вспомогательная функция для сохранения первого слова в команде.
-- Конечный автомат сохраняет состояния в виде цепочки
-- >>> prependChar 'c' =<< prependChar 'm' =<< pure (Command [['d']])
prependChar :: MonadError m => Char -> Primitive -> m Primitive
prependChar c (Assignment name value) = flip Assignment value <$> addChar c name
prependChar c (Command [])            = return $ Command [[c]]
prependChar c (Command (w : ws))      = return . Command $ (c : w) : ws

-- | Чтение значения переменной в присваивании.
parseValue :: MonadError m => String -> m String
parseValue s = splitBySpaces s >>= \case
  [] -> pure ""
  [x] -> pure x
  (_ : _) -> throwError $ error' "Command calls with variable overriding are not supported"
  . filter (not . null)

-- | Разбиение произвольной строки по пробелам
-- с учётом кавычек (возможны пустые строки,
-- используется в связке с @filter (not . null)@.
splitBySpaces :: MonadError m => String -> m [String]
splitBySpaces  ""                = pure [""]
splitBySpaces  "\\"              = throwError $ error' "Unexpected end of line after \\"
splitBySpaces ('\\' : '\\' : cs) = headMap ('\\' :) <$> splitBySpaces cs
splitBySpaces ('\\' : '\"' : cs) = headMap ('\"' :) <$> splitBySpaces cs
splitBySpaces ('\\' : '\'' : cs) = headMap ('\'' :) <$> splitBySpaces cs
splitBySpaces ('\\' : ' '  : cs) = headMap (' ' :) <$> splitBySpaces cs
splitBySpaces (       '\'' : cs) = singleQuotes cs >>= \(s, r) -> headMap (s ++) <$> splitBySpaces r
splitBySpaces (       '\"' : cs) = doubleQuotes cs >>= \(s, r) -> headMap (s ++) <$> splitBySpaces r
splitBySpaces (       ' '  : cs) = ("" :) <$> splitBySpaces cs
splitBySpaces (       '\t' : cs) = ("" :) <$> splitBySpaces cs
splitBySpaces (        c   : cs) = headMap (c :) <$> splitBySpaces cs

-- | Применение функции только к голове списка,
-- если она есть.
headMap :: (a -> a) -> [a] -> [a]
headMap _ []       = []
headMap f (x : xs) = f x : xs

-- | Чтение фрагмента строки, заключённого в одинарные кавычки.
singleQuotes :: MonadError m => String -> m (String, String)
singleQuotes "" = throwError $ error' "Unexpected end of line in single quotes"
singleQuotes ('\'' : cs) = pure ("", cs)
singleQuotes ( c   : cs) = first (c :) <$> singleQuotes cs

-- | Чтение фрагмента строки, заключённого в двойные кавычки.
doubleQuotes :: MonadError m => String -> m (String, String)
doubleQuotes "" = throwError $ error' "Unexpected end of line in double quotes"
doubleQuotes ('\\' : '\\' : cs) = first ('\\' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\"' : cs) = first ('\"' :) <$> doubleQuotes cs
doubleQuotes ('\\' : '\'' : cs) = first ('\'' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 'n'  : cs) = first ('\n' :) <$> doubleQuotes cs
doubleQuotes ('\\' : 't'  : cs) = first ('\t' :) <$> doubleQuotes cs
doubleQuotes (       '\"' : cs) = pure ("", cs)
doubleQuotes (        c   : cs) = first (c :) <$> doubleQuotes cs
