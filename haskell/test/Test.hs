module Main where
import Data.Monoid (mempty)
import Shrdlite.Common as Common
import Shrdlite.Grammar
import Shrdlite.Interpreter
import Shrdlite.Planner
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.JSON


-- Properties to implement
-- - A plan must not contain to "take" actions after another
--

main :: IO ()
main = defaultMainWithOpts
    [ testGroup "interpretTests"
        [ testCase "interpreterTest1" interpretTest1
        , testCase "interpreterTest2" interpretTest2
        ]
    , testGroup "findEntityTests"
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
        , testCase "validateObjectTest4" validateObjectTest4
        , testCase "validateObjectTest5" validateObjectTest5
        , testCase "validateObjectTest6" validateObjectTest6
        , testCase "validateObjectTest7" validateObjectTest7
        ]
    , testGroup "checkTests"
        [ testCase "checkTest1" checkTest1
        , testCase "checkTest2" checkTest2
        , testCase "checkTest3" checkTest3
        , testCase "checkTest4" checkTest4
        , testCase "checkTest5" checkTest4
        , testCase "checkTest6" checkTest4
        , testCase "checkTest7" checkTest4
        , testCase "checkTest8" checkTest4
        ]
    ] mempty

-- |Tests the interpreter
interpretTest1 :: Assertion
interpretTest1 = assertBool "Interpret failed." (length result == 1 && (head result) == MoveGoal Ontop (Obj The "f") Flr)
  where cmd = Move (RelativeEntity The anyBall (Relative Inside (BasicEntity The (Object AnySize Blue Box)))) (Relative Ontop Floor)
        Right result = interpret startState cmd

-- |Tests the interpreter
interpretTest2 :: Assertion
interpretTest2 = assertBool "Interpret failed." (isEntityError result)
  where cmd =	Move (BasicEntity The (Object AnySize AnyColor Ball)) (Relative Inside (RelativeEntity The (Object AnySize Blue Box) (Relative Ontop Floor)))
        Left result = interpret startState cmd

-- |Tries to find any white large ball.
findEntityTest1 :: Assertion
findEntityTest1 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where Right foundEntities = unInterpret startState $ findEntity $ anyEntity largeWhiteBall

-- |Tries to find any white large ball which is next to a table of any size and
-- any color.
findEntityTest2 :: Assertion
findEntityTest2 = assertBool "Could not find object" ("e" `elem` foundEntities)
    where Right foundEntities = unInterpret startState $ findEntity $ allRelativeEntity largeWhiteBall besideTableLocation

-- |Tries to find any large black ball.
findEntityTest3 :: Assertion
findEntityTest3 = assertBool "Found an object not present in the world" (length foundEntities == 0)
    where Right foundEntities = unInterpret startState $ findEntity $ anyEntity largeBlackBall

-- |Tries to find any large white ball which is next to a small brick of any
-- size and any color.
findEntityTest4 :: Assertion
findEntityTest4 = assertBool ("Expected no matching objects, but found: " ++ show foundEntities) (length foundEntities == 0)
    where Right foundEntities = unInterpret startState $ findEntity $ anyRelativeEntity largeWhiteBall besideSmallBrickLocation

-- |Checks ambiguities
findEntityTest5 :: Assertion
findEntityTest5 = assertBool "Expected ambiguity, but received a correct result." isAmbiguity
    where foundEntities = unInterpret startState $ findEntity $ BasicEntity The (Object AnySize AnyColor Ball)
          isAmbiguity = case foundEntities of
                          Left _  -> True
                          Right _ -> False

-- |Tests Beside relation
locationHoldsTest1 :: Assertion
locationHoldsTest1 = assertBool "Beside location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "l"
          obj      = Object Large AnyColor Box
          loc      = Relative Beside (BasicEntity Any largeWhiteBall)

-- |Tests Leftof relation
locationHoldsTest2 :: Assertion
locationHoldsTest2 = assertBool "Leftof location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "e"
          obj      = largeWhiteBall
          loc      = Relative Leftof (BasicEntity Any (Object Large AnyColor Box))

-- |Tests Rightof relation
locationHoldsTest3 :: Assertion
locationHoldsTest3 = assertBool "Rightof location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "l"
          obj      = Object Large AnyColor Box
          loc      = Relative Rightof (BasicEntity Any largeWhiteBall)

-- |Tests Above relation
locationHoldsTest4 :: Assertion
locationHoldsTest4 = assertBool "Above location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "f"
          obj      = smallBlackBall
          loc      = Relative Above (BasicEntity Any (Object AnySize AnyColor Box))

-- |Tests Ontop relation
locationHoldsTest5 :: Assertion
locationHoldsTest5 = assertBool "Ontop location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "l"
          obj      = Object Large AnyColor Box
          loc      = Relative Ontop (BasicEntity Any (Object AnySize AnyColor Table))

-- |Tests Under relation
locationHoldsTest6 :: Assertion
locationHoldsTest6 = assertBool "Under location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "m"
          obj      = Object Small Blue Box
          loc      = Relative Under (BasicEntity Any (Object AnySize AnyColor Ball))

-- |Tests Inside relation
locationHoldsTest7 :: Assertion
locationHoldsTest7 = assertBool "Inside location expected to hold, but doesn't" locHolds
    where Right locHolds = unInterpret startState $ locationHolds (id, obj) loc
          id       = "f"
          obj      = smallBlackBall
          loc      = Relative Inside (BasicEntity Any (Object AnySize AnyColor Box))

searchObjectsTest1 :: Assertion
searchObjectsTest1 = assertBool ("Expected to find " ++ show expected ++ " but found " ++ show found) (expected == found)
    where expected = Right [("e", largeWhiteBall), ("f", smallBlackBall)] :: Either InterpretationError [(Id, Object)]
          found    = unInterpret startState $ searchObjects (Object AnySize AnyColor Ball) All Nothing

findObjPosTest1 :: Assertion
findObjPosTest1 = assertBool "Object with id \"e\" was not found in column 0 and height 0" ((findObjPos "e" (world startState)) == Just (0,0))

findObjPosTest2 :: Assertion
findObjPosTest2 = assertBool "Object with id \"g\" was not found in column 1 and height 0" ((findObjPos "g" (world startState)) == Just (1,0))

findObjPosTest3 :: Assertion
findObjPosTest3 = assertBool "Object with id \"l\" was not found in column 1 and height 1" ((findObjPos "l" (world startState)) == Just (1,1))

-- | Tests if we can't place something on a ball
validateObjectTest1 :: Assertion
validateObjectTest1 = assertBool "I managed to place a large box on a large ball" $ not $ validate largeRedBox largeWhiteBall

-- | Tests if placing a large object on a small object returns false
validateObjectTest2 :: Assertion
validateObjectTest2 = assertBool "I managed to place a large object on a small object" $ not $ validate largeRedBox smallGreenBox

-- | Testa whether we can place a large plank on top of a small brick
validateObjectTest3 :: Assertion
validateObjectTest3 = assertBool "I managed to place a large plank on a small brick" $ not $ validate (Object Large Red Plank) (Object Small White Brick)

-- | Checks that nothing can be placed on a ball
validateObjectTest4 :: Assertion
validateObjectTest4 = assertBool "I managed to place something on a ball"
  $ and $ map f $ map xs forms
    where
      f :: Object -> Bool
      f = not . flip validate (Object Small White Ball)

      xs :: Form -> Object
      xs x = (Object Large White x)

-- | Checks if every large object cannot be placed on every small object
validateObjectTest5 :: Assertion
validateObjectTest5 = assertBool "I managed to place a large object on a small one"
  $ and $ [(f l s) |  l <- map (oc Large) forms, s <- map (oc Small) forms]
    where
      f :: Object -> Object -> Bool
      f lo so = not $ validate lo so

      oc :: Size -> Form -> Object
      oc sz frm = (Object sz White frm)

-- | Boxes cannot contain pyramids or planks of the same size
validateObjectTest6 :: Assertion
validateObjectTest6 = assertBool "I managed to place a plank/pyramid inside a box"
  $ and $ [(f p (oc Large Box)) | p <- map (oc Large) [Pyramid,Plank]]
       ++ [(f p (oc Small Box)) | p <- map (oc Small) [Pyramid,Plank]]
       ++ [(validate p (oc Large Box)) | p <- map (oc Small) [Pyramid,Plank]]
    where
      f :: Object -> Object -> Bool
      f po bo = not $ validate po bo

      oc :: Size -> Form -> Object
      oc sz frm = (Object sz White frm)

-- | Boxes can only be supported by tables or planks of the same size, but large boxes can also be supported by large bricks
validateObjectTest7 :: Assertion
validateObjectTest7 = assertBool "I managed to place a box on something other than a table or plank (or large brick for large box)"
  $ and $ [(validate (oc Large Box) p) | p <- map (oc Large) [Table,Plank,Brick]]
       ++ [(validate (oc Small Box) p) | p <- map (oc Small) [Table,Plank]]
       ++ [(not $ validate (oc Small Box) p) | p <- map (oc Large) [Table,Plank,Brick]]
       ++ [(not $ validate (oc Large Box) p) | p <- map (oc Small) [Table,Plank]]
    where
      oc :: Size -> Form -> Object
      oc sz frm = (Object sz White frm)

-- | Tests if a take goal is fullfilled when holding the requested object
checkTest1 :: Assertion
checkTest1 = assertBool "I could not see that I'm already holding the requested object" $
   check (TakeGoal (Obj The "a")) (smallWorld, Just "a")

-- | Tetss if it finds the correct id of what it is holding
checkTest2 :: Assertion
checkTest2 = assertBool "I thought I held something that I didn't" $
   and $ [ not $ check (TakeGoal (Obj The "a")) (smallWorld, Just "b")
         , not $ check (TakeGoal (Obj The "a")) (smallWorld, Nothing)]

-- | Tests Ontop Goal
checkTest3 :: Assertion
checkTest3 = assertBool "I could not verify the Ontop relation" $
   and $ [ check (MoveGoal Ontop (Obj The "a") (Obj The "b")) ([["b", "a"], []], Nothing)
         , not $ check (MoveGoal Ontop (Obj The "a") (Obj The "b")) ([["a"],["b"]], Nothing)]

-- | Tests Beside Goal
checkTest4 :: Assertion
checkTest4 = assertBool "I could not verify the Beside relation" $
   and $ [ check (MoveGoal Beside (Obj The "a") (Obj The "b")) ([["a"], ["b"]], Nothing)
         , not $ check (MoveGoal Beside (Obj The "a") (Obj The "b")) ([["a", "b"], []], Nothing)]

-- | Tests Leftof Goal
checkTest5 :: Assertion
checkTest5 = assertBool "I could not verify the Leftof relation" $
   and $ [ check (MoveGoal Leftof (Obj The "a") (Obj The "b")) ([["a"], [], ["b"]], Nothing)
         , not $ check (MoveGoal Leftof (Obj The "a") (Obj The "b")) ([["a", "b"], []], Nothing)]

-- | Tests Rightof Goal
checkTest6 :: Assertion
checkTest6 = assertBool "I could not verify the Rightof relation" $
   and $ [ check (MoveGoal Rightof (Obj The "a") (Obj The "b")) ([["b"], [], ["a"]], Nothing)
         , not $ check (MoveGoal Rightof (Obj The "a") (Obj The "b")) ([["a", "b"], []], Nothing)]

-- | Tests Above Goal
checkTest7 :: Assertion
checkTest7 = assertBool "I could not verify the Above relation" $
   and $ [ check (MoveGoal Above (Obj The "a") (Obj The "c")) ([["c", "b", "a"], []], Nothing)
         , not $ check (MoveGoal Above (Obj The "a") (Obj The "b")) ([["a"], ["b"]], Nothing)]

-- | Tests Under Goal
checkTest8 :: Assertion
checkTest8 = assertBool "I could not verify the Under relation" $
   and $ [ check (MoveGoal Under (Obj The "a") (Obj The "c")) ([["a", "b", "c"], []], Nothing)
         , not $ check (MoveGoal Under (Obj The "a") (Obj The "b")) ([["a"], ["b"]], Nothing)]

-- | A list of all the given forms used to create all possible objects.
forms :: [Form]
forms = [Brick,Plank,Ball,Pyramid,Box,Table]

largeWhiteBall :: Object
largeWhiteBall = Object Large White Ball

largeBlackBall :: Object
largeBlackBall = Object Large Black Ball

smallBlackBall :: Object
smallBlackBall = Object Small Black Ball

largeRedBox :: Object
largeRedBox = Object Large Red Box

anyBall :: Object
anyBall = Object AnySize AnyColor Ball

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

smallWorld :: World
smallWorld = [["e"], ["g", "l"], [], ["k", "m", "f"], []]

testWorld :: String -> String
testWorld utterance = "{ \"world\": [ [ \"e\" ], [ \"g\", \"l\" ], [], [ \"k\", \"m\", \"f\" ], [] ], \"objects\": { \"a\": { \"form\": \"brick\", \"size\": \"large\", \"color\": \"green\" }, \"b\": { \"form\": \"brick\", \"size\": \"small\", \"color\": \"white\" }, \"c\": { \"form\": \"plank\", \"size\": \"large\", \"color\": \"red\" }, \"d\": { \"form\": \"plank\", \"size\": \"small\", \"color\": \"green\" }, \"e\": { \"form\": \"ball\", \"size\": \"large\", \"color\": \"white\" }, \"f\": { \"form\": \"ball\", \"size\": \"small\", \"color\": \"black\" }, \"g\": { \"form\": \"table\", \"size\": \"large\", \"color\": \"blue\" }, \"h\": { \"form\": \"table\", \"size\": \"small\", \"color\": \"red\" }, \"i\": { \"form\": \"pyramid\", \"size\": \"large\", \"color\": \"yellow\" }, \"j\": { \"form\": \"pyramid\", \"size\": \"small\", \"color\": \"red\" }, \"k\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"yellow\" }, \"l\": { \"form\": \"box\", \"size\": \"large\", \"color\": \"red\" }, \"m\": { \"form\": \"box\", \"size\": \"small\", \"color\": \"blue\" } }, \"holding\": null, \"utterance\":" ++ show (words utterance) ++ "}"
