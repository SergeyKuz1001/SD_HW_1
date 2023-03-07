{- |
В данном модуле объявлены все фазы работы данной программы.
-}
module Phases (
    analyzer,
    executor,
    parser,
    stringReader,
  ) where

import Phases.Analyzer
import Phases.Executor
import Phases.Parser
import Phases.StringReader
