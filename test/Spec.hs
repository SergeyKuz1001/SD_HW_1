import           Phases.Analyzer.Tests (testsAnalyzer)
import           Phases.Parser.Tests   (testsParser)
import           Phases.Executor.Tests   (testsExecutor)

import           Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList [
    TestLabel "Analyzer" testsAnalyzer,
    TestLabel "Parser" testsParser,
    TestLabel "Executor" testsExecutor
  ]
