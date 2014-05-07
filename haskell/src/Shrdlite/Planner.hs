module Shrdlite.Planner where

import Shrdlite.Common

import qualified Data.Set as S
import qualified Data.List as L
import Data.Graph.AStar
import Data.Maybe

-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Maybe Id -> Objects -> Goal -> Plan
solve world holding objects goal = statePlan plan
  where
    state = State { world=world, holding=holding, objects=objects }
    solution = aStarSolve (state,goal)
    plan = (state, goal) : (fromMaybe [] solution)

{-
    [ "I totally picked it up . . ."
    , "pick " ++ show col
    , ". . . and I dropped it down."
    , "drop " ++ show col ]
    where
      Just col = case goal of
        (TakeGoal (Obj i)) -> fmap fst $ findObjPos i world
      -- Just col = findIndex (not . null) world
-}

-- Given a state, this produces all possible neighbours
stateGraph :: (State,Goal) -> S.Set (State,Goal)
stateGraph (state, goal) = case holding state of
  Just id -> foldl (placeObject state goal id) S.empty worldParts -- TODO: foldl' or foldr instead
  Nothing -> foldl (takeObject state goal) S.empty worldParts -- TODO: foldl' or foldr instead
  where
    w = world state
    worldParts = init $ zip (L.inits w) (L.tails w) -- TODO: try highest column first, so sort the zipped lists

placeObject :: State -> Goal -> Id -> S.Set (State, Goal) -> ([[Id]], [[Id]]) -> S.Set (State, Goal)
placeObject state goal h s e = case newWorld h of
  Nothing -> s
  Just w ->
    if validState newState
    then S.insert newState s
    else s
      where
        newState = (state {holding=Nothing, world=w},goal)
  where
    newWorld elem = joinModified e (\x -> x ++ [elem])

takeObject :: State -> Goal -> S.Set (State, Goal) -> ([[Id]], [[Id]]) -> S.Set (State, Goal)
takeObject state goal s e = case takeHighest e of
  Nothing -> s
  Just id -> S.insert (state {holding=Just id, world=newWorld},goal) s
  where
    newWorld = fromJust $ joinModified e maybeInit
    maybeInit [] = []
    maybeInit xs = init xs

-- Checks if a state is valid
validState :: (State, Goal) -> Bool
validState _ = True -- TODO: actually check :)

-- Joins two parts of the world together, modifying the first element in the
-- second list. The function is what modifies the element (which is itself a
-- list) and is normally either placing or taking an object from the top of
-- the list. Returns a new world
joinModified :: ([[Id]], [[Id]]) -> ([Id] -> [Id]) -> Maybe [[Id]]
joinModified ([], []) f = Nothing
joinModified (xs, []) f = Just xs
joinModified (xs, y:ys) f = Just $ xs ++ f y:ys

takeHighest :: ([[Id]], [[Id]]) -> Maybe Id
takeHighest ([], []) = Nothing
takeHighest (xs, []) = maybeLast $ last xs
takeHighest (xs, y:ys) = maybeLast y

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

-- The distance between two neitgboring nodes, i.e. two states.
-- This is always 1 for now, but could match column difference etc.
dist :: (State,Goal) -> (State,Goal) -> Int
dist = const.const 1

-- Heuristic defining how good a state is
heuristics :: (State,Goal) -> Int
heuristics (state, goal) = case goal of
  TakeGoal (Flr _)  -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i)  -> case holding state of
    Nothing             -> idHeight i (world state)
    Just holdingId      ->
      if i == holdingId
      then 0
      else (+) 1 $ idHeight i (world state)
  PutGoal {}        -> error "PutGoal not implemented yet"

idHeight :: Id -> World -> Int
idHeight _ [] = error "object is not in the world"
idHeight i (w:[]) = ((length w) - 1) -(fromJust $ L.elemIndex i w)
idHeight i (w:ws) = case L.elemIndex i w of
  Nothing     -> idHeight i ws
  Just index  -> ((length w) - 1) - index

-- | Checks if the goal is fulfilled in the provided state
check :: (State,Goal) -> Bool
check (state, goal) = case goal of
  TakeGoal (Flr _)  -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i)  -> case holding state of
    Nothing             -> False
    Just holdingId      -> i == holdingId
  PutGoal {}        -> error "PutGoal not implemented yet"

aStarSolve :: (State,Goal) -> Maybe [(State,Goal)]
aStarSolve = aStar stateGraph dist heuristics check

statePlan :: [(State,Goal)] -> Plan
statePlan ((s,_):[]) = []
statePlan ((s1,_):(s2,g):xs) = statePlan ((s2,g):xs) ++ stateTransition w1 w2 0
  where w1 = world s1
        w2 = world s2

stateTransition :: World -> World -> Int -> Plan
stateTransition [] [] _ = error "stateTransition: no changes"
stateTransition (c1:c1s) (c2:c2s) col = case compare (length c1) (length c2) of
  LT -> ["drop " ++ show col]
  GT -> ["pick " ++ show col]
  EQ -> stateTransition c1s c2s (col + 1)

-- | Checks which plans is the best one, according to some heuristic
bestPlan :: [Plan] -> Plan
bestPlan = undefined

