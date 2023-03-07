{- |
В данном модуле объявлены примитивы, в которые транслируется пользовательский
запрос после этапа анализа.
-}
module Data.ImprovedPrimitive (
    Primitive(..),
    Command(..),
    Special(..),
    Common(..),
    Internal(..),
    External(..),
  ) where

import Environment.FSPrimitive (AbsFilePath)

-- | Примитив является командой (пустой или нет).
data Primitive = Command Command | EmptyCommand
  deriving (Eq, Show)

-- | Команда — это объект, который может быть выполнен.
-- Команда может быть либо специальной (влиять на работу оболочки), либо
-- обычной.
data Command = Special Special | Common Common
  deriving (Eq, Show)

-- | Специальная команда влияет на работу оболочки. Таковой командой является
-- только команда @exit@, которая принимает опциональным параметром код
-- возврата из оболочки.
data Special = Exit (Maybe Int)
  deriving (Eq, Show)

-- | Обычная команда имеет потоки ввода, вывода и поток ошибок.
-- Может быть либо внутренней (исполняется самой командной оболочкой), либо
-- внешней.
data Common = Internal Internal | External External
  deriving (Eq, Show)

-- | Внутренняя команда исполняется самой командной оболочкой и может быть
--
--     * @cat@ — вывод содержимого переданного файла;
--     * @echo@ — вывод аргументов через пробел;
--     * @wc@ — статистика для файла;
--     * @pwd@ — имя текущей директории.
data Internal = Cat AbsFilePath | Echo [String] | Wc AbsFilePath | Pwd
  deriving (Eq, Show)

-- | Внешняя команда вызывается по пути к исполняемому файлу с указанными
-- аргументами.
data External = Arguments AbsFilePath [String]
  deriving (Eq, Show)
