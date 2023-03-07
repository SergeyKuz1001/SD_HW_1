{- |
В данном модуле объявлена монада @'MonadExit'@, предоставляющая возможность
выхода из данной программы.
-}
module Environment.MonadExit (
    ExitCode,
    MonadExit(..),
  ) where

-- | Код возврата (как в POSIX).
type ExitCode = Int

-- | Монада для выхода из программы с указанным кодом возврата.
class Monad m => MonadExit m where
  exit :: ExitCode -> m ()
