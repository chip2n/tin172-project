module Shrdlite.Planner where

import Shrdlite.Common

import qualified Data.Set as S
import Data.Graph.AStar

-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Maybe Id -> Objects -> Goal -> Plan
solve world holding objects goal =
    [ "I totally picked it up . . ."
    , "pick " ++ show col
    , ". . . and I dropped it down."
    , "drop " ++ show col ]
    where
      Just col = case goal of
        (TakeGoal (Obj i)) -> fmap fst $ findObjPos i world
      -- Just col = findIndex (not . null) world

stateGraph :: (State,Goal) -> S.Set (State,Goal)
stateGraph = undefined

-- The distance between two neitgboring nodes, i.e. two states.
-- This is always 1 for now, but could match column difference etc.
dist :: (State,Goal) -> (State,Goal) -> Int
dist = const.const 1

-- Heuristic defining how good a state is
heuristics :: (State,Goal) -> Int
heuristics = undefined

-- | Checks if the goal is fulfilled in the provided state
check :: (State,Goal) -> Bool
check = undefined

aStarSolve :: (State,Goal) -> Maybe [(State,Goal)]
aStarSolve = aStar stateGraph dist heuristics check

-- | Checks which plans is the best one, according to some heuristic
bestPlan :: [Plan] -> Plan
bestPlan = undefined

