{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
В данном модуле объявлена монада @'MonadError'@ с основными функциями для
простой работы с ней.
-}
module Environment.MonadError (
    Error(..),
    MonadError,
    (?:),
    (@:),
    (?>=),
    (@>=),
    throwError,
    catchError,
  ) where

import Control.Monad.Except hiding (MonadError, MonadIO)
import qualified Control.Monad.Except as ME
import System.Console.ANSI (setSGRCode, SGR(..), ConsoleLayer(..), ConsoleIntensity(..), ColorIntensity(..), Color(..))

-- | Тип стандартной (для нашей системы) ошибки. Хранит стадию, на которой эта
-- ошибка произошла, а также дополнительную информацию.
data Error = Error String String

instance Show Error where
  show (Error type_ msg) =
    setSGRCode [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] ++
    type_ ++
    setSGRCode [Reset] ++ ": " ++ msg

-- | Синоним @'ME.MonadError' 'Error'@.
class ME.MonadError Error m => MonadError m

instance MonadError (Either Error)

infix 0 ?:, @:, ?>=, @>=

-- | Операция для вызова исключения если написанное слева выражение ложно.
(?:) :: MonadError m => Bool -> Error -> m ()
False ?: err = throwError err
True  ?: _   = return ()

-- | Операция для вызова исключения если написанное слева выражение не
-- является значением.
(@:) :: MonadError m => Maybe a -> Error -> m a
Nothing    @: err = throwError err
(Just res) @: _   = return res

-- | Аналог операции @'(?:)'@ для выражения в монаде.
(?>=) :: MonadError m => m Bool -> Error -> m ()
m ?>= err = m >>= (?: err)

-- | Аналог операции @'(\@:)'@ для выражения в монаде.
(@>=) :: MonadError m => m (Maybe a) -> Error -> m a
m @>= err = m >>= (@: err)
