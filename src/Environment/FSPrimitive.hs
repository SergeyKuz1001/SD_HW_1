{- |
В данном модуле объявлены основные примитивы для платформонезависимой работы с
файловой системой.
-}
module Environment.FSPrimitive (
    AbsFilePath (asFilePath),
    absFilePath,
    isBaseName,
    (</>),
    Permissions (..),
    File (..),
  ) where

import Environment.MonadError (Error(..), MonadError, (?:))

import qualified System.FilePath as FP

infixl 6 </>

-- | Абсолютный путь к файлу. Главный инвариант этого типа — абсолютность пути,
-- однако довольно часто можно утверждать, что это корректный путь к
-- существующей папке или файлу.
newtype AbsFilePath = AbsFilePath { asFilePath :: FilePath }
  deriving (Eq, Ord)

instance Show AbsFilePath where
  show (AbsFilePath path) = path

-- | Функция для конструирования значения типа @'AbsFilePath'@. При
-- конструировании проверяется соблюдение инварианта, и в случае его нарушения
-- вызывается ошибка.
absFilePath :: MonadError m => FilePath -> m AbsFilePath
absFilePath path = do
  FP.isAbsolute path ?: Error "ViolationOfInvariantError" (
    "\"" ++ path ++ "\" is not absolute path")
  return $ AbsFilePath path

-- | Функция проверки того, является ли путь простым именем файла или нет.
isBaseName :: FilePath -> Bool
isBaseName = notElem FP.pathSeparator

-- | Операция конкатенации пути. В отличие от @'FP.</>'@ конкатенируются
-- абсолютный и относительный пути, получая в итоге абсолютный.
(</>) :: AbsFilePath -> FilePath -> AbsFilePath
(</>) (AbsFilePath path) = AbsFilePath . (path FP.</>)

-- | Разрешения файла.
data Permissions = Permissions
  { readPerm  :: Bool
  , writePerm :: Bool
  , execPerm  :: Bool
  }

-- | Файл в файловой системе — это абсолютный путь до него и разрешения.
data File = File
  { filePath    :: AbsFilePath
  , permissions :: Permissions
  }
