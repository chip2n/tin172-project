module Shrdlite.Planner where

import Shrdlite.Common
import Shrdlite.Grammar

import Data.Graph.AStar
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Maybe Id -> Objects -> Goal -> Plan
solve w h o goal = statePlan plan
  where
    state = State { world=w, holding=h, objects=o}
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
  Just i  -> foldl (placeObject state goal i) S.empty worldParts -- TODO: foldl' or foldr instead
  Nothing -> foldl (takeObject state goal) S.empty worldParts -- TODO: foldl' or foldr instead
  where
    w = world state
    worldParts = init $ zip (L.inits w) (L.tails w) -- TODO: try highest column first, so sort the zipped lists

placeObject :: State -> Goal -> Id -> S.Set (State, Goal) -> ([[Id]], [[Id]]) -> S.Set (State, Goal)
placeObject state goal h s e = case newWorld h of
  Nothing -> s
  Just w -> S.insert newState s
      where
        newState = (state {holding=Nothing, world=w},goal)
  where
    newWorld elm = joinModified state e (\x -> x ++ [elm])

takeObject :: State -> Goal -> S.Set (State, Goal) -> ([[Id]], [[Id]]) -> S.Set (State, Goal)
takeObject state goal s e = case takeHighest e of
  Nothing -> s
  Just i  -> S.insert (state {holding=Just i, world=newWorld},goal) s
  where
    --newWorld = fromJust $ joinModified state e maybeInit
    newWorld = case joinModified state e maybeInit of
      Nothing -> error "Error: Physical laws violated"
      Just e' -> e'
    maybeInit [] = []
    maybeInit xs = init xs

-- Checks if a state is valid
validState :: (State, Goal) -> Bool
validState _ = True -- TODO: actually check :)

-- Joins two parts of the world together, modifying the first element in the
-- second list. The function is what modifies the element (which is itself a
-- list) and is normally either placing or taking an object from the top of
-- the list. Returns a new world
joinModified :: State -> ([[Id]], [[Id]]) -> ([Id] -> [Id]) -> Maybe [[Id]]
joinModified _ ([], []) _ = Nothing
joinModified _ (_, []) _ = Nothing
joinModified state (xs, y:ys) f = case val state (f y) of
   Nothing -> Nothing
   Just y' -> Just $ xs ++ y':ys
--if length y' > 1 && head y' == "c" && last y' == "b"
--              then error $ show y'

-- Taeks a column and checks if the the two topmost objects are in the correct
-- order. If there are less than two object we assume there can be no conflicts.
val :: State -> [Id] -> Maybe [Id]
val state y' | length y' > 1 = case l y1 of
                  Nothing  -> Nothing
                  Just y1' -> case l y2 of
                     Nothing  -> Nothing
                     Just y2' -> if (validate y1' y2')
                                 then Just y'
                                 else Nothing
             | otherwise = Just y'
      where l ys'  = M.lookup ys' objs
            [y1, y2] = take 2 $ reverse y'
            objs = objects state

-- Tests whether the first object can be placed immediately above the second
-- object without violating the given constraints.
validate :: Object -> Object -> Bool
validate _                         (Object _     _ Ball)  = False
validate (Object s1     _ Ball)    (Object s2    _ Box)   = s1 <= s2
validate (Object _      _ Ball)    _                      = False
validate (Object s1     _ Pyramid) (Object s2    _ Box)   = s1 < s2
validate (Object s1     _ Plank)   (Object s2    _ Box)   = s1 < s2
validate (Object s1     _ Box)     (Object s2    _ Plank) = s1 == s2
validate (Object s1     _ Box)     (Object s2    _ Table) = s1 == s2
validate (Object Large  _ Box)     (Object Large _ Brick) = True
validate (Object s1     _ Box)     (Object s2    _ Box  ) = s1 < s2 -- TODO wrong?
validate (Object _      _ Box)     _                      = False
validate (Object s1     _ _)       (Object s2    _ _)     = s1 <= s2

takeHighest :: ([[Id]], [[Id]]) -> Maybe Id
takeHighest ([], []) = Nothing
takeHighest (xs, []) = maybeLast $ last xs
takeHighest (_, y:_) = maybeLast y

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
  TakeGoal Flr -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i) -> case holding state of
    Nothing             -> fromMaybe (error "object is not in the world")
                                   $ idHeight i (world state)
    Just holdingId      ->
      if i == holdingId
      then 0
      else fromMaybe (error "object is not in the world")
                   $ idHeight i (world state)
  PutGoal Ontop (Obj i) Flr -> case getFloorSpace (world state) of
    Just flrNum -> 1
    Nothing -> case makeFloorSpace (world state) of
      Just steps -> minimum steps
      Nothing -> error $ "makeFloorSpace: can't make room for object: " ++ i
  PutGoal {} -> error "PutGoal while not hold an object isn't implemented yet."

getFloorSpace :: World -> Maybe Int
getFloorSpace [] = error "TODO: All floor spaces are taken"
getFloorSpace ([]:_) = return 0
getFloorSpace (_:ws) = case getFloorSpace ws of
  Just i -> return $ i+1
  Nothing -> error "TODO: ALl floor spaces are taken"

makeFloorSpace :: World -> Maybe [Int]
makeFloorSpace [] = return []
makeFloorSpace (w:ws) = do
  let l = length w
  ls <- makeFloorSpace ws
  return (l:ls)

idHeight :: Id -> World -> Maybe Int
idHeight _ [] = error "object is not in the world"
--idHeight i (w:[]) = return $ ((length w) - 1) -(fromMaybe Nothing $ L.elemIndex i w)
idHeight i (w:[]) = case L.elemIndex i w of
  Nothing    -> Nothing
  Just index -> return $ ((length w) - 1) - index
idHeight i (w:ws) = case L.elemIndex i w of
  Nothing     -> idHeight i ws
  Just index  -> return $ ((length w) - 1) - index

-- | Checks if the goal is fulfilled in the provided state
check :: (State,Goal) -> Bool
check (state, goal) = case goal of
  TakeGoal (Flr)  -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i)  -> case holding state of
    Nothing             -> False
    Just holdingId      -> i == holdingId
  PutGoal Ontop (Obj i) Flr -> case idHeight i (world state) of
    Just height -> height == 0
    Nothing -> False
  PutGoal {}      -> error "PutGoal not fully implemented yet"

aStarSolve :: (State,Goal) -> Maybe [(State,Goal)]
aStarSolve = aStar stateGraph dist heuristics check

statePlan :: [(State,Goal)] -> Plan
statePlan ((_,_):[]) = []
statePlan ((s1,_):(s2,g):xs) = stateTransition w1 w2 0 ++ statePlan ((s2,g):xs)
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

