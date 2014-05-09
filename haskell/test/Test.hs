module Main where
import Data.Monoid (mempty)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Shrdlite.Interpreter
import Text.JSON
import Shrdlite.Common as Common
import Shrdlite.Grammar


-- Properties to implement
-- - A plan must not contain to "take" actions after another
--

main :: IO ()
main = defaultMainWithOpts
    [ testGroup "findEntityTests"
        [ testCase "findEntityTest1" findEntityTest1
        , testCase "findEntityTest2" findEntityTest2
        , testCase "findEntityTest3" findEntityTest3
        , testCase "findEntityTest4" findEntityTest4
        , testCase "findEntityTest5" findEntityTest5
        ]
    , testGroup "locationHoldsTests"
        [ testCase "locationHoldsTest1" locationHoldsTest1
        , testCase "locationHoldsTest2" locationHoldsTest2
        , testCase "locationHoldsTest3" locationHoldsTest3
        , testCase "locationHoldsTest4" locationHoldsTest4
        , testCase "locationHoldsTest5" locationHoldsTest5
        , testCase "locationHoldsTest6" locationHoldsTest6
        , testCase "locationHoldsTest7" locationHoldsTest7
        ]
    , testGroup "searchObjectsTests"
        [ testCase "searchObjectsTest1" searchObjectsTest1
        ]
    , testGroup "findObjPosTests"
        [ testCase "findObjPosTest1" findObjPosTest1
        , testCase "findObjPosTest2" findObjPosTest2
        , testCase "findObjPosTest3" findObjPosTest3
        ]
    , testGroup "validateObjectTest"
        [ testCase "validateObjectTest1" validateObjectTest1
        , testCase "validateObjectTest2" validateObjectTest2
        , testCase "validateObjectTest3" validateObjectTest3
        ]
    ] mempty

-- |Tries to find any white large ball.
findEntityTest1 :: Assertion
findEntityTest1 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where Right foundEntities = findEntity startState $ anyEntity largeWhiteBall

-- |Tries to find any white large ball which is next to a table of any size and
-- any color.
findEntityTest2 :: Assertion
findEntityTest2 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where Right foundEntities = findEntity startState $ allRelativeEntity largeWhiteBall besideTableLocation

-- |Tries to find any large black ball.
findEntityTest3 :: Assertion
findEntityTest3 = assertBool "Found an object not present in the world" (length foundEntities == 0)
    where Right foundEntities = findEntity startState $ anyEntity largeBlackBall

-- |Tries to find any large white ball which is next to a small brick of any
-- size and any color.
findEntityTest4 :: Assertion
findEntityTest4 = assertBool ("Expected no matching objects, but found: " ++ show foundEntities) (length foundEntities == 0)
    where Right foundEntities = findEntity startState $ anyRelativeEntity largeWhiteBall besideSmallBrickLocation

-- |Checks ambiguities
findEntityTest5 :: Assertion
findEntityTest5 = assertBool "Expected ambiguity, but received a correct result." isAmbiguity
    where foundEntities = findEntity startState $ BasicEntity The (Object AnySize AnyColor Ball)
          isAmbiguity = case foundEntities of
                          Left _  -> True
                          Right _ -> False

-- |Tests Beside relation
locationHoldsTest1 :: Assertion
locationHoldsTest1 = assertBool "Beside location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "l"
          obj      = Object Large AnyColor Box 
          loc      = Relative Beside (BasicEntity Any largeWhiteBall)

-- |Tests Leftof relation
locationHoldsTest2 :: Assertion
locationHoldsTest2 = assertBool "Leftof location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "e"
          obj      = largeWhiteBall
          loc      = Relative Leftof (BasicEntity Any (Object Large AnyColor Box))

-- |Tests Rightof relation
locationHoldsTest3 :: Assertion
locationHoldsTest3 = assertBool "Rightof location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "l"
          obj      = Object Large AnyColor Box 
          loc      = Relative Rightof (BasicEntity Any largeWhiteBall)

-- |Tests Above relation
locationHoldsTest4 :: Assertion
locationHoldsTest4 = assertBool "Above location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "f"
          obj      = smallBlackBall
          loc      = Relative Above (BasicEntity Any (Object AnySize AnyColor Box))

-- |Tests Ontop relation
locationHoldsTest5 :: Assertion
locationHoldsTest5 = assertBool "Ontop location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "l"
          obj      = Object Large AnyColor Box 
          loc      = Relative Ontop (BasicEntity Any (Object AnySize AnyColor Table))

-- |Tests Under relation
locationHoldsTest6 :: Assertion
locationHoldsTest6 = assertBool "Under location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "m"
          obj      = Object Small Blue Box
          loc      = Relative Under (BasicEntity Any (Object AnySize AnyColor Ball))

-- |Tests Inside relation
locationHoldsTest7 :: Assertion
locationHoldsTest7 = assertBool "Inside location expected to hold, but doesn't" locHolds
    where locHolds = locationHolds startState (id, obj) loc
          id       = "f"
          obj      = smallBlackBall
          loc      = Relative Inside (BasicEntity Any (Object AnySize AnyColor Box))

searchObjectsTest1 :: Assertion
searchObjectsTest1 = assertBool ("Expected to find " ++ show expected ++ " but found " ++ show found) (expected == found)
    where expected = Right [("e", largeWhiteBall), ("f", smallBlackBall)] :: Either [[(Id, Object)]] [(Id, Object)]
          found    = searchObjects startState (Object AnySize AnyColor Ball) All Nothing

findObjPosTest1 :: Assertion
findObjPosTest1 = assertBool "Object with id \"e\" was not found in column 0 and height 0" ((findObjPos "e" (world startState)) == Just (0,0))

findObjPosTest2 :: Assertion
findObjPosTest2 = assertBool "Object with id \"g\" was not found in column 1 and height 0" ((findObjPos "g" (world startState)) == Just (1,0))

findObjPosTest3 :: Assertion
findObjPosTest3 = assertBool "Object with id \"l\" was not found in column 1 and height 1" ((findObjPos "l" (world startState)) == Just (1,1))

-- | Test if a large ball can be placed in a large box
validateObjectTest1 :: Assertion
validateObjectTest1 = undefined
--validateObjectTest1 = assertBool "The large ball can't be placed in a large box" $ validate largeWhiteBall largeRedBox

-- | Tests if we can't place something on a ball
validateObjectTest2 :: Assertion
validateObjectTest2 = undefined
--validateObjectTest2 = assertBool "I managed to place a large box on a large ball" $ validate largeRedBox largeWhiteBall

-- | Tests if placing a large object on a small object returns false
validateObjectTest3 :: Assertion
validateObjectTest3 = undefined
--validateObjectTest3 = assertBool "I managed to place a large object on a small object" $ validate largeRedBox smallGreenBox

--Boxes cannot contain pyramids or planks of the same size
--Boxes can only be supported by tables or planks of the same size, but large boxes can also be supported by large bricks

largeWhiteBall :: Object
largeWhiteBall = Object Large White Ball

largeBlackBall :: Object
largeBlackBall = Object Large Black Ball

smallBlackBall :: Object
smallBlackBall = Object Small Black Ball

largeRedBox :: Object
largeRedBox = Object Large Red Box

smallGreenBox :: Object
smallGreenBox = Object Small Green Box

besideTableLocation :: Location
besideTableLocation = Relative Beside (BasicEntity Any (Object AnySize AnyColor Table))

besideSmallBrickLocation :: Location
besideSmallBrickLocation = Relative Beside (BasicEntity Any (Object Small AnyColor Brick))

anyEntity :: Object -> Entity
anyEntity obj = BasicEntity Any obj

anyRelativeEntity :: Object -> Location -> Entity
anyRelativeEntity obj loc = RelativeEntity Any obj loc

allRelativeEntity :: Object -> Location -> Entity
allRelativeEntity obj loc = RelativeEntity All obj loc

startState :: Common.State
startState = Common.State world holding objects
    where
        world   = (ok (valFromObj "world" (ok . decode $ testWorld "take the white ball"))) :: World
        holding = Nothing :: Maybe Id
        objects = parseObjects $ (ok (valFromObj "objects" (ok . decode $ testWorld "take the white ball"))) :: Objects

testWorld :: String -> String
testWorld utterance = "{ \"world\": [ [ \"e\" ], [ \"g\", \"l\" ], [], [ \"k\", \"m\", \"f\" ], [] ], \"objects\": { \"a\": { \"form\": \"brick\", \"size\": \"large\", \"color\": \"green\" }, \"b\": { \"form\": \"brick\", \"size\": \"small\", \"color\": \"white\" }, \"c\": { \"form\": \"plank\", \"size\": \"large\", \"color\": \"red\" }, \"d\": { \"form\": \"plank\", \"size\": \"small\", \"color\": \"green\" }, \"e\": { \"form\": \"ball\", \"size\": \"large\", \"color\": \"white\" }, \"f\": { \"form\": \"ball\", \"size\": \"small\", \"color\": \"black\" }, \"g\": { \"form\": \"table\", \"size\": \"large\", \"color\": \"blue\" }, \"h\": { \"form\": \"table\", \"size\": \"small\", \"color\": \"red\" }, \"i\": { \"form\": \"pyramid\", \"size\": \"large\", \"color\": \"yellow\" }, \"j\": { \"form\": \"pyramid\", \"size\": \"small\", \"color\": \"red\" }, \"k\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"yellow\" }, \"l\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"red\" }, \"m\": { \"form\": \"box\", \"size\": \"small\", \"color\": \"blue\" } }, \"holding\": null, \"utterance\":" ++ show (words utterance) ++ "}"
