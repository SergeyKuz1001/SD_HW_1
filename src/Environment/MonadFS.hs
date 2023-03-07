{- |
В данном модуле объявлена монада @'MonadFS'@ для платформонезависимой работы с
файловой системой.
-}
module Environment.MonadFS (
    MonadFS (..),
    findFile,
    findFileAsExecutable,
    doesFileExist,
    doesExecutableExist,
    isReadable,
    isWritable,
    isExecutable,
  ) where

import Environment.FSPrimitive
import Environment.MonadVarPathReader (MonadVarPathReader(..))
import Environment.MonadVarPwdReader (MonadVarPwdReader(..))

import Control.Applicative (asum)

-- | Монада для поиска файлов в файловой системе.
class Monad m => MonadFS m where
  -- | Поиск файла по абсолютному пути.
  findFileByAbsPath :: AbsFilePath -> m (Maybe File)

-- | Поиск файла в текущей директории (если указан относительный путь) или в
-- корневой директории (если указан абсолютный).
findFile :: (MonadFS m, MonadVarPwdReader m) => FilePath -> m (Maybe File)
findFile path = case absFilePath path of
  Left _ -> do
    pwd <- getVarPwd
    findFileByAbsPath $ pwd </> path
  Right absPath ->
    findFileByAbsPath absPath

-- | Поиск файла, аналогичный @'findFile'@, но с использованием переменной PATH
-- (через монаду @'MonadVarPathReader'@) если указано только имя файла.
findFileAsExecutable :: (MonadFS m, MonadVarPwdReader m, MonadVarPathReader m) => FilePath -> m (Maybe File)
findFileAsExecutable path = case absFilePath path of
  Left _ -> do
    pwd <- getVarPwd
    binLocations <-
      if isBaseName path
        then return [pwd]
        else (pwd :) <$> getVarPath
    fmap asum . traverse (findFileByAbsPath . (</> path)) $ binLocations
  Right absPath ->
    findFileByAbsPath absPath

-- | Проверка на то, что файл по этому пути (абсолютному или относительному)
-- существует. Если файл существует, то возвращается абсолютный путь до него.
doesFileExist :: (MonadFS m, MonadVarPwdReader m) => FilePath -> m (Maybe AbsFilePath)
doesFileExist path = fmap filePath <$> findFile path

-- | Аналогично @'doesFileExist'@, но при этом происходит поиск с использованием
-- переменной PATH и проверка, что найденный файл может быть выполнен.
doesExecutableExist :: (MonadFS m, MonadVarPwdReader m, MonadVarPathReader m) => FilePath -> m (Maybe AbsFilePath)
doesExecutableExist path = do
  mFile <- findFileAsExecutable path
  if fmap (execPerm . permissions) mFile == Just True
    then return $ filePath <$> mFile
    else return Nothing

-- | Проверка на то, что разрешение файла удовлетворяет предикату. Возвращает
-- @False@ если файл не существует.
hasPerm :: MonadFS m => (Permissions -> Bool) -> AbsFilePath -> m Bool
hasPerm p absPath = do
  mFile <- findFileByAbsPath absPath
  let result = p . permissions <$> mFile
  return $ result == Just True

-- | Проверка на то, что файл может быть прочитан. Возвращает @False@ если файл
-- не существует.
isReadable :: MonadFS m => AbsFilePath -> m Bool
isReadable = hasPerm readPerm

-- | Проверка на то, что файл может быть перезаписан. Возвращает @False@ если
-- файл не существует.
isWritable :: MonadFS m => AbsFilePath -> m Bool
isWritable = hasPerm writePerm

-- | Проверка на то, что файл может быть выполнен. Возвращает @False@ если файл
-- не существует.
isExecutable :: MonadFS m => AbsFilePath -> m Bool
isExecutable = hasPerm execPerm
