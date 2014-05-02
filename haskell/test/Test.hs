module Main where
import Data.Monoid (mempty)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Shrdlite.Interpreter
import Text.JSON
import Shrdlite.Planner as Planner
import Shrdlite.Common as Common
import Shrdlite.Interpreter as Interpreter
import Shrdlite.Grammar
import qualified Data.Map as Map


-- Properties to implement
-- - A plan must not contain to "take" actions after another
--

main :: IO ()
main = defaultMainWithOpts
    [ testGroup "findEntityTests"
        [ testCase "findEntityTest1" findEntityTest1
        , testCase "findEntityTest2" findEntityTest2
        , testCase "findEntityTest3" findEntityTest3
        ]
    ] mempty

-- | Tries to find any white large ball.
findEntityTest1 :: Assertion
findEntityTest1 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where obj           = Object Large White Ball
          entity        = BasicEntity Any obj
          foundEntities = findEntity startState entity

-- | Tries to find any white large ball which is next to a table of any size and
-- any color.
findEntityTest2 :: Assertion
findEntityTest2 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where obj           = Object Large White Ball
          loc           = Relative Beside (BasicEntity Any (Object AnySize AnyColor Table))
          entity        = RelativeEntity All obj loc
          foundEntities = findEntity startState entity

-- | Tries to find any large black ball.
findEntityTest3 :: Assertion
findEntityTest3 = assertBool "Found an object not present in the world" (length foundEntities == 0)
    where obj           = Object Large Black Ball
          entity        = BasicEntity Any obj
          foundEntities = findEntity startState entity

startState :: Common.State
startState = Common.State world holding objects
    where
        world   = (ok (valFromObj "world" (ok . decode $ testWorld "take the white ball"))) :: World
        holding = Nothing :: Maybe Id
        objects = parseObjects $ (ok (valFromObj "objects" (ok . decode $ testWorld "take the white ball"))) :: Objects

testWorld :: String -> String
testWorld utterance = "{ \"world\": [ [ \"e\" ], [ \"g\", \"l\" ], [], [ \"k\", \"m\", \"f\" ], [] ], \"objects\": { \"a\": { \"form\": \"brick\", \"size\": \"large\", \"color\": \"green\" }, \"b\": { \"form\": \"brick\", \"size\": \"small\", \"color\": \"white\" }, \"c\": { \"form\": \"plank\", \"size\": \"large\", \"color\": \"red\" }, \"d\": { \"form\": \"plank\", \"size\": \"small\", \"color\": \"green\" }, \"e\": { \"form\": \"ball\", \"size\": \"large\", \"color\": \"white\" }, \"f\": { \"form\": \"ball\", \"size\": \"small\", \"color\": \"black\" }, \"g\": { \"form\": \"table\", \"size\": \"large\", \"color\": \"blue\" }, \"h\": { \"form\": \"table\", \"size\": \"small\", \"color\": \"red\" }, \"i\": { \"form\": \"pyramid\", \"size\": \"large\", \"color\": \"yellow\" }, \"j\": { \"form\": \"pyramid\", \"size\": \"small\", \"color\": \"red\" }, \"k\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"yellow\" }, \"l\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"red\" }, \"m\": { \"form\": \"box\", \"size\": \"small\", \"color\": \"blue\" } }, \"holding\": null, \"utterance\":" ++ show (words utterance) ++ "}"
