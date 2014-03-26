#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where 

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)

type Utterance = [String]
type Id = String
type World = [[Id]]
type Objects = JSObject JSValue
type Goal = [(Relation, GoalObject, GoalObject)]
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
      objects   = ok (valFromObj "objects"   jsinput) :: Objects

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

showGoals :: [Goal] -> [String]
showGoals goals = map show goals

-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: World -> Id -> Objects -> Command -> [Goal]
interpret world holding objects tree = [[(Ontop, Obj "a", Flr 0)]]

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

