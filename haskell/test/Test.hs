module Main where
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit
import Shrdlite.Interpreter
import Text.JSON
import Shrdlite.Planner as Planner
import Shrdlite.Common as Common
import Shrdlite.Interpreter as Interpreter
import ShrdliteGrammar
import Main


-- Properties to implement
-- - A plan must not contain to "take" actions after another
--

main :: IO ()
main = defaultMainWithOpts [ testGroup "findEntityTests" [
                                                         testCase "findEntityTest1" findEntityTest1,
                                                         testCase "findEntityTest2" findEntityTest2
                                                         ]] mempty


--data Entity = Floor | BasicEntity Quantifier Object | RelativeEntity Quantifier Object Location
--data Location = Relative Relation Entity
--findEntityTest1 =  (assertEqual "lol" 1 1)
findEntityTest1 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where obj           = Object Ball Large White
          loc           = Relative Beside (BasicEntity Any (Object Table AnySize AnyColor))
          entity        = RelativeEntity All obj loc
          foundEntities = findEntity startState entity
findEntityTest2 = 2 @?= 1

startState :: Common.State
startState = Common.State world holding objects
    where
        world   = (ok (valFromObj "world" (ok . decode $ testWorld "take the white ball"))) :: World
        holding = Nothing :: Maybe Id
        objects = parseObjects $ (ok (valFromObj "objects" (ok . decode $ testWorld "take the white ball"))) :: Objects

testWorld :: String -> String
testWorld utterance = "{ \"world\": [ [ \"e\" ], [ \"g\", \"l\" ], [], [ \"k\", \"m\", \"f\" ], [] ], \"objects\": { \"a\": { \"form\": \"brick\", \"size\": \"large\", \"color\": \"green\" }, \"b\": { \"form\": \"brick\", \"size\": \"small\", \"color\": \"white\" }, \"c\": { \"form\": \"plank\", \"size\": \"large\", \"color\": \"red\" }, \"d\": { \"form\": \"plank\", \"size\": \"small\", \"color\": \"green\" }, \"e\": { \"form\": \"ball\", \"size\": \"large\", \"color\": \"white\" }, \"f\": { \"form\": \"ball\", \"size\": \"small\", \"color\": \"black\" }, \"g\": { \"form\": \"table\", \"size\": \"large\", \"color\": \"blue\" }, \"h\": { \"form\": \"table\", \"size\": \"small\", \"color\": \"red\" }, \"i\": { \"form\": \"pyramid\", \"size\": \"large\", \"color\": \"yellow\" }, \"j\": { \"form\": \"pyramid\", \"size\": \"small\", \"color\": \"red\" }, \"k\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"yellow\" }, \"l\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"red\" }, \"m\": { \"form\": \"box\", \"size\": \"small\", \"color\": \"blue\" } }, \"holding\": null, \"utterance\":" ++ show (words utterance) ++ "}"
