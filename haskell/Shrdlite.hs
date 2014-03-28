#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where 

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)
import qualified Data.Map as M

type Utterance = [String]
type Id = String
type World = [[Id]]
--type Objects = JSObject JSValue
type Objects = M.Map Id Object
data Goal = TakeGoal GoalObject
          | PutGoal Relation GoalObject GoalObject
          deriving (Show)
data GoalObject = Flr Int | Obj Id deriving (Show)
type Plan = [String]


main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode


jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where 
      utterance = ok (valFromObj "utterance" jsinput) :: Utterance
      world     = ok (valFromObj "world"     jsinput) :: World
      holding   = ok (valFromObj "holding"   jsinput) :: Id
      objects   = parseObjects $ ok (valFromObj "objects"   jsinput) :: Objects

      trees     = parse command utterance :: [Command]

      goals     = [goal | tree <- trees, goal <- interpret world holding objects tree] :: [Goal]

      plan      = solve world holding objects (head goals) :: Plan

      output
        | null trees = "Parse error!"
        | null goals = "Interpretation error!"
        | length goals >= 2 = "Ambiguity error!"
        | null plan = "Planning error!"
        | otherwise = "Much wow!"

      result    = [("utterance", showJSON utterance),
                   ("trees",     showJSON (map show trees)),
                   ("goals",     if not (null trees) then showJSON (showGoals goals) else JSNull),
                   ("plan",      if length goals == 1 then showJSON plan  else JSNull),
                   ("output",    showJSON output),
                   ("receivedJSON", showJSON $ jsinput)
                  ]


parseObjects :: JSObject JSValue -> Objects
parseObjects obj = M.fromList $ map parseValue $ fromJSObject obj

parseValue :: (Id, JSValue) -> (Id, Object)
parseValue (s, JSObject val) = (s, obj)
 where
    objForm = head $ parse form' $ [ok (valFromObj "form" val) :: String]
    objSize = head $ parse size $ [ok (valFromObj "size" val) :: String]
    objColor = head $ parse color $ [ok (valFromObj "color" val) :: String]
    obj = Object objSize objColor objForm

form' :: SParser Form
form' = lexicon [(Brick,   ["brick"]),
                 (Plank,   ["plank"]),
                 (Ball,    ["ball"]),
                 (Pyramid, ["pyramid"]),
                 (Box,     ["box"]),
                 (Table,   ["table"])]

showGoals :: [Goal] -> [String]
showGoals goals = map show goals

-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: World -> Id -> Objects -> Command -> [Goal]
--interpret world holding objects tree = [[(Ontop, Obj "a", Flr 0)]]
interpret world holding objects (Take entity) =
    case entity of
        Floor                    -> error "Cannot take floor, ye rascal!"
        BasicEntity q obj        -> 
            let found = matchingObjects q obj Nothing
            in  map (\(i,o) -> TakeGoal (Obj i)) found
        RelativeEntity q obj loc -> 
            let found = matchingObjects q obj (Just loc)
            in  map (\(i,o) -> TakeGoal (Obj i)) found
  where matchingObjects q obj l = searchObjects world holding objects obj q l
interpret world holding objects goal = undefined


-- | Searches the objects map after objects matching the quantifier and location
searchObjects :: World -> Id -> Objects -> Object -> Quantifier -> Maybe Location -> [(Id, Object)]
searchObjects world holding objects obj quantifier loc =
    case loc of
        Nothing ->
            case quantifier of
                All -> foundObjects
                Any -> take 1 foundObjects
                The -> take 1 foundObjects --TODO: Assert only one element
        Just location -> 
            case quantifier of
                All -> undefined
                Any -> undefined
                The -> undefined
  where 
        ids = holding : concat world
        exists (id,_)     = id `elem` ids
        isMatching (_, o) = obj == o
        foundObjects = filter exists . filter isMatching $ M.assocs objects


-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Id -> Objects -> Goal -> Plan
solve world holding objects goal = ["I totally picked it up . . .", "pick " ++ show col, ". . . and I dropped it down.", "drop " ++ show col]
    where
      Just col = findIndex (not . null) world


-- | Checks if the goal is fulfilled in the world state
check :: World -> Goal -> Bool
check = undefined

-- | Checks which plans is the best one, according to some heuristic
bestPlan :: [Plan] -> Plan
bestPlan = undefined

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

