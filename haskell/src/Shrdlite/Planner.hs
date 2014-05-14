module Shrdlite.Planner where

import Shrdlite.Common
import Shrdlite.Grammar
import Shrdlite.AStar

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Maybe Id -> Objects -> Goal -> Plan
solve w h o goal = fromMaybe [] plan
  where
    plan = aStar o (worldGraph o) (heuristics goal) (check goal) (w,h)

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
worldGraph :: Objects -> WorldHolding -> S.Set WorldHolding
worldGraph o (w, h) = case h of
  Just i  -> foldl (placeObject o i) S.empty worldParts -- TODO: foldl' or foldr instead
  Nothing -> foldl (takeObject o) S.empty worldParts -- TODO: foldl' or foldr instead
  where
    worldParts = init $ zip (L.inits w) (L.tails w) -- TODO: try highest column first, so sort the zipped lists

placeObject :: Objects -> Id -> S.Set WorldHolding -> ([[Id]], [[Id]]) -> S.Set WorldHolding
placeObject o h s e = case newWorld h of
  Nothing -> s
  Just w -> S.insert (w, Nothing) s
  where
    newWorld elm = joinModified o e (\x -> x ++ [elm])

takeObject :: Objects -> S.Set WorldHolding -> ([[Id]], [[Id]]) -> S.Set WorldHolding
takeObject o s e = case takeHighest e of
  Nothing -> s
  Just i  -> S.insert (newWorld, Just i) s
  where
    --newWorld = fromJust $ joinModified state e maybeInit
    newWorld = fromMaybe (error "Error: Physical laws violated")
                         (joinModified o e maybeInit)
    maybeInit [] = []
    maybeInit xs = init xs

-- Joins two parts of the world together, modifying the first element in the
-- second list. The function is what modifies the element (which is itself a
-- list) and is normally either placing or taking an object from the top of
-- the list. Returns a new world
joinModified :: Objects -> ([[Id]], [[Id]]) -> ([Id] -> [Id]) -> Maybe [[Id]]
joinModified _ ([], []) _ = Nothing
joinModified _ (_, []) _ = Nothing
joinModified objs (xs, y:ys) f = case val objs (f y) of
   Nothing -> Nothing
   Just y' -> Just $ xs ++ y':ys
--if length y' > 1 && head y' == "c" && last y' == "b"
--              then error $ show y'

-- Taeks a column and checks if the the two topmost objects are in the correct
-- order. If there are less than two object we assume there can be no conflicts.
val :: Objects -> [Id] -> Maybe [Id]
val objs y' | length y' > 1 = case l y1 of
                  Nothing  -> Nothing
                  Just y1' -> case l y2 of
                     Nothing  -> Nothing
                     Just y2' -> if validate y1' y2'
                                 then Just y'
                                 else Nothing
             | otherwise = Just y'
      where l ys'  = M.lookup ys' objs
            [y1, y2] = take 2 $ reverse y'

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
validate (Object s1     _ Box)     (Object s2    _ Box  ) = s1 < s2
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
heuristics :: Goal -> WorldHolding -> Int
heuristics goal (w, h) = case goal of
  TakeGoal Flr -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i) -> case h of
    Nothing             -> fromMaybe (error "object is not in the world2")
                                   $ idHeight i w
    Just holdingId      ->
      if i == holdingId
      then 0
      else fromMaybe (error "object is not in the world")
                   $ idHeight i w
  PutGoal Ontop (Obj i) Flr -> case getFloorSpace w of
    Just _ -> 1
    Nothing -> case makeFloorSpace w of
      Just steps -> minimum steps
      Nothing -> error $ "makeFloorSpace: can't make room for object: " ++ i
  PutGoal rel (Obj i1) (Obj i2) -> case rel of
    Ontop ->  let h1 = fromMaybe 0 $ idHeight i1 w
                  h2 = fromMaybe 0 $ idHeight i2 w
              in h1 + h2
    Inside -> let h1 = fromMaybe 0 $ idHeight i1 w
                  h2 = fromMaybe 0 $ idHeight i2 w
              in h1 + h2
    Beside -> let c1 = fromMaybe 1 $ clmn 0 i1 w
                  c2 = fromMaybe 1 $ clmn 0 i2 w
                  l1 = w !! c1
                  l2 = w !! c2
                  h1 = fromMaybe 0 $ idHeight' i1 l1
                  h2 = fromMaybe 0 $ idHeight' i2 l2
              in (*) 2 $ minimum [length l2 - h2,length l1 - h1] 
    _ -> error $ "TODO: impelement " ++ show rel ++ " in heuristics\n"
              ++ "Trying to put " ++ i1 ++ " " ++ show rel ++ " " ++ i2
  PutGoal {} -> error "PutGoal while not hold an object isn't implemented yet."

objAbove :: Id -> World -> Maybe Int
objAbove _ [] = error "objects aren't in the world!"
objAbove i2 (w:[]) = case L.elemIndex i2 w of
  Nothing -> Nothing
  Just index ->  return (length w - index)
objAbove i2 (w:ws) = case L.elemIndex i2 w of
  Nothing -> objAbove i2 ws
  Just index ->  return index -- 2 * (length w - 1 - index)

getFloorSpace :: World -> Maybe Int
getFloorSpace [] = Nothing
getFloorSpace ([]:_) = return 0
getFloorSpace (_:ws) = case getFloorSpace ws of
  Just i -> return $ i+1
  Nothing -> Nothing

-- TODO: Probably an incorrect interpretation of the number of steps.
makeFloorSpace :: World -> Maybe [Int]
makeFloorSpace [] = return []
makeFloorSpace (w:ws) = makeFloorSpace ws >>= \ls -> return $ 2 * length w : ls

clmn :: Int -> Id -> World -> Maybe Int
clmn _ _ [] = Nothing
clmn row i (w:ws) = case L.elemIndex i w of
                      Nothing -> clmn (row+1) i ws
                      Just _  -> return row

-- | Gives an int for how far down the object is
idHeight :: Id -> World -> Maybe Int
idHeight _ [] = Nothing
idHeight i (w:ws) = case idHeight' i w of
  Nothing -> idHeight i ws
  mVal    -> mVal
--idHeight i (w:ws) = case L.elemIndex i w of
--  Nothing     -> idHeight i ws
--  Just index  -> return $ (length w - 1) - index

idHeight' :: Id -> [Id] -> Maybe Int
idHeight' i ws = case L.elemIndex i ws of
  Nothing -> Nothing
  Just index -> return $ (length ws - 1) - index

objPos :: Id -> World -> Maybe Int
objPos _ [] = Nothing
objPos i (w:ws) = case L.elemIndex i w of
  Nothing -> objPos i ws
  Just index -> return index

-- | Checks is the first object is above the second object in the world
isAbove :: Id -> Id -> World -> Bool
isAbove _ _ [] = False
isAbove i1 i2 (w:ws) = case L.elemIndex i2 w of
  Nothing -> isAbove i1 i2 ws
  Just index -> let i = index + 1 -- Index above i2, where i1 should be
                in ((length w > i) -- Only look of i2 if list is long enough
                  && (i1 == w !! i)) -- Is i1 above i2?

-- | Checks if the first object is over the second, with some maximum distance
isOver :: Id -> Id -> Int -> World -> Bool
isOver _ _ _ [] = False
isOver i1 i2 distance (w:ws) = case L.elemIndex i1 w of
  Nothing     -> isOver i1 i2 distance ws
  Just index1 -> case L.elemIndex i2 w of
    Nothing     -> False
    Just index2 -> let d = index1 - index2
                   in d > 0 && d <= distance

-- | Checks if the goal is fulfilled in the provided state
check :: Goal -> WorldHolding -> Bool
check goal (w, h) = case goal of
  TakeGoal Flr  -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i)  -> case h of
    Nothing             -> False
    Just holdingId      -> i == holdingId
  PutGoal Ontop (Obj i) Flr -> case objPos i w of
    Just height -> height == 0
    Nothing -> False
  PutGoal rel (Obj i1) (Obj i2) -> case rel of
    Ontop -> isOver i1 i2 1 w
    Inside -> isOver i1 i2 1 w
    Beside -> isBeside i1 i2 w
    _ -> error $ "Not implemented yet: Trying to put " ++ i1 ++ " " ++ show rel ++ " of " ++ i2
  PutGoal {}      -> error "PutGoal not fully implemented yet"

-- TODO: Might want to check same height
isBeside :: Id -> Id -> World -> Bool
isBeside _ _ [] = False
isBeside _ _ [_] = False
isBeside i1 i2 (w:v:ws) = if isHere i1 i2 w
                            then isHere i1 i2 v
                            else isBeside i1 i2 (v:ws)
  where
    isHere _ _ [] = False
    isHere i j (o:os) = (i == o || j == o) || isHere i j os

statePlan :: [(State,Goal)] -> Plan
statePlan []                 = []
statePlan ((_,_):[])         = []
statePlan ((s1,_):(s2,g):xs) = stateTransition w1 w2 0 ++ statePlan ((s2,g):xs)
  where w1 = world s1
        w2 = world s2

stateTransition :: World -> World -> Int -> Plan
stateTransition (c1:c1s) (c2:c2s) col = case compare (length c1) (length c2) of
  LT -> ["drop " ++ show col]
  GT -> ["pick " ++ show col]
  EQ -> stateTransition c1s c2s (col + 1)
stateTransition _ _ _ = error "stateTransition: no changes"

-- | Checks which plans is the best one, according to some heuristic
bestPlan :: [Plan] -> Plan
bestPlan = undefined

