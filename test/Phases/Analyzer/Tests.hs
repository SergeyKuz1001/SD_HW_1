module Phases.Analyzer.Tests (
    testsAnalyzer,
  ) where

import qualified Data.Primitive as P
import Data.ImprovedPrimitive hiding (Primitive(..))
import qualified Data.ImprovedPrimitive as IP
import Phases.Analyzer (analyzer)
import Phases.Analyzer.TestEnvironment

import Test.HUnit hiding (test, path)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = foldr (\z -> if x == z then (y:) else (z:)) []

absFilePath' :: String -> AbsFilePath
absFilePath' path = case absFilePath path of
  Left _ -> either undefined id . absFilePath $ "C:" ++ replace '/' '\\' path
  Right absPath -> absPath

env1 :: TestEnvironment a -> Either Error a
env1 = runTestEnvironment (absFilePath' "/home/user/") [
    File (absFilePath' "/bin/vim") $ Permissions True True True,
    File (absFilePath' "/home/user/Documents/lessons_schedule.txt") $ Permissions True True False,
    File (absFilePath' "/home/user/my_game.py") $ Permissions True True True,
    File (absFilePath' "/home/user/.vimrc") $ Permissions True True False,
    File (absFilePath' "/GitHub_PASSWORD.txt") $ Permissions True True False
  ]

test :: (Eq b, Show b) => (TestEnvironment IP.Primitive -> Either Error b) -> [String] -> Maybe b -> Test
test _ [] _ = TestCase $ fail "Incorrect test"
test f (command : args) expected = TestCase $
  (eitherToMaybe . f) (analyzer . P.Command $ command : args) @?= expected
    where
      eitherToMaybe (Right x) = Just x
      eitherToMaybe (Left _) = Nothing

testsAnalyzer :: Test
testsAnalyzer = TestList [
    test env1 ["exit", "6"] $
      Just (IP.Command . Special . Exit $ Just 6),
    test env1 ["echo", "1", "2"] $
      Just (IP.Command . Common . Internal $ Echo ["1", "2"]),
    test env1 ["cat", ".vimrc"] $
      Just (IP.Command . Common . Internal . Cat $ absFilePath' "/home/user/.vimrc"),
    test env1 ["cat", asFilePath $ absFilePath' "/GitHub_PASSWORD.txt"] $
      Just (IP.Command . Common . Internal . Cat $ absFilePath' "/GitHub_PASSWORD.txt"),
    test env1 ["pwd"] $
      Just (IP.Command . Common . Internal $ Pwd),
    test env1 [asFilePath $ absFilePath' "/bin/vim", "-O", ".vimrc", "my_game.py"] $
      Just (IP.Command . Common . External $ Arguments (absFilePath' "/bin/vim") ["-O", ".vimrc", "my_game.py"]),
    test env1 ["my_game.py"] $
      Just (IP.Command . Common . External $ Arguments (absFilePath' "/home/user/my_game.py") []),
    test env1 ["exit", "hahaha"] Nothing,
    test env1 ["cat"] Nothing,
    test env1 ["cat", "123"] Nothing,
    test env1 ["wc", ".vimrc", "my_game.py"] Nothing,
    test env1 ["emacs"] Nothing,
    test env1 ["pwd", "."] Nothing
  ]
