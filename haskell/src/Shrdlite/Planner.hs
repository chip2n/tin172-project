module Shrdlite.Planner where

import Shrdlite.Common
import Shrdlite.Grammar
import Shrdlite.AStar

import Data.Maybe
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Maybe Id -> Objects -> [Goal] -> Plan
solve _ _ _ []     = []
solve w h o (g:[]) = fromMaybe [] plan
  where
    plan = aStar (worldGraph o) (heuristics g) (check g) (w,h)
solve w h o (g:gs) = case plan of
                        Nothing -> []
                        Just p  ->  case newPlan of
                           [] -> []
                           p' -> p ++ p'
                           where (newWorld, newHolding) = simulatePlan w h p
                                 newPlan = solve newWorld newHolding o gs
  where
    plan = aStar (worldGraph o) (heuristics g) (check g) (w,h)

simulatePlan :: World -> Maybe Id -> Plan -> (World, Maybe Id)
simulatePlan oldWorld h [] = (oldWorld, h)
simulatePlan oldWorld (Just h) (p:ps) = simulatePlan newWorld Nothing ps
   where (left, col:right) = L.splitAt (read (last (words p))::Int) oldWorld
         newWorld = left ++ [col ++ [h]] ++ right
simulatePlan oldWorld Nothing (p:ps) = simulatePlan newWorld (Just c) ps
   where (left, (c:col):right) = L.splitAt (read (last (words p))::Int) oldWorld
         newWorld = left ++ col:right


-- Given a state, this produces all possible neighbours
worldGraph :: Objects -> WorldHolding -> S.Set WorldHolding
worldGraph o (w, h) = case h of
  Just i  -> foldr (placeObject o i) S.empty worldParts
  Nothing -> foldr (takeObject o) S.empty worldParts
  where
    worldParts = init $ zip (L.inits w) (L.tails w) -- TODO: try highest column first, so sort the zipped lists

placeObject :: Objects -> Id -> ([[Id]], [[Id]]) -> S.Set WorldHolding -> S.Set WorldHolding
placeObject o h e s = case newWorld h of
  Nothing -> s
  Just w -> S.insert (w, Nothing) s
  where
    newWorld elm = joinModified o e (\x -> x ++ [elm])

takeObject :: Objects -> ([[Id]], [[Id]]) -> S.Set WorldHolding -> S.Set WorldHolding
takeObject o e s = case takeHighest e of
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
heuristics (TakeGoal obj) (w, h) = case obj of
  Flr -> error "Take floor goal cannot be assessed"
  (Obj i) -> case h of
    Nothing             -> 1 + 2 * fromMaybe (error "object is not in the world2")
                                   (idHeight i w)
    Just holdingId      ->
      if i == holdingId
      then 0
      else 2 + 2 * fromMaybe (error "object is not in the world")
                   (idHeight i w)
heuristics (MoveGoal Ontop (Obj i) Flr) (w,h) = if getFloorSpace w
  then case h of
         Nothing -> if hght == 0 then 0
                                 else 2 + 2 * oAbove
         Just holdingId | i == holdingId -> 1
                        | hght == 0      -> 0
                        | otherwise      -> 2 + 2 * oAbove
  else 
    case h of
      Nothing ->  if hght == 0
                    then 0
                    else 2 * hght + minimum fs
      Just holdingId | i == holdingId -> 3 + minimum fs -- put it down, move the smallest pile and then pick it up and put it on the floor.
                     | hght == 0      -> 0 -- Is our object already on the floor? good
                     | otherwise      -> 3 + 2 * oAbove + minimum fs -- put the holding down, clear the objects above, move the smallest pile.
  where oAbove = fromMaybe (error "object isn't in the world") (idHeight i w)
        hght = fromMaybe (error "planner: where did the object go?") (objPos i w)
        fs = makeFloorSpace w
heuristics (MoveGoal rel (Obj i1) (Obj i2)) (w,h) = case rel of
  Ontop ->  let h1 = fromMaybe 1 $ liftM (*2) $ idHeight i1 w
                h2 = fromMaybe 1 $ liftM (*2) $ idHeight i2 w
            in case h of
                 Nothing        -> h1 + h2
                 Just holdingId -> if holdingId == i1 || holdingId == i2
                                     then h1 + h2
                                     else h1 + h2 + 2 -- remove the object we're holding.
  Inside -> let h1 = fromMaybe 0 $ liftM (*2) $ idHeight i1 w
                h2 = fromMaybe 0 $ liftM (*2) $ idHeight i2 w
            in case h of
                 Nothing        -> h1 + h2
                 Just holdingId -> if holdingId == i1 || holdingId == i2
                                     then h1 + h2
                                     else h1 + h2 + 2
  Beside -> case h of
    Nothing        -> cheapestCost i1 i2 w
    Just holdingId -> if holdingId == i1 || holdingId == i2
                        then 1
                        else 2 + cheapestCost i1 i2 w
  Leftof -> case h of
    Nothing        -> cheapestCost i1 i2 w
    Just holdingId -> if holdingId == i1 || holdingId == i2
                        then 1
                        else 2 + cheapestCost i1 i2 w
  Rightof -> case h of
    Nothing        -> cheapestCost i1 i2 w
    Just holdingId -> if holdingId == i1 || holdingId == i2
                        then 1
                        else 2 + cheapestCost i1 i2 w
  Above -> case h of
    Nothing        -> cheapestCost i1 i2 w
    Just holdingId -> if holdingId == i1 || holdingId == i2
                        then 1
                        else 2 + cheapestCost i1 i2 w
  Under -> case h of
    Nothing        -> cheapestCost i1 i2 w
    Just holdingId -> if holdingId == i1 || holdingId == i2
                        then 1
                        else 2 + cheapestCost i1 i2 w
  --_ -> error $ "TODO: impelement " ++ show rel ++ " in heuristics\n"
  --          ++ "Trying to put " ++ i1 ++ " " ++ show rel ++ " " ++ i2
--heuristic (PutGoal {}) _ = error "PutGoal while not hold an object isn't implemented yet."

-- | Returns the cheapest heuristics to either move all objects above one of the
-- two given objects.
cheapestCost :: Id -> Id -> World -> Int
cheapestCost i1 i2 w = (*) 2 $ minimum [h1,h2]
  where h1 = fromMaybe 1 $ idHeight i1 w
        h2 = fromMaybe 1 $ idHeight i2 w

-- | Checks whether there exits an empty floor space.
getFloorSpace :: World -> Bool
getFloorSpace [] = False
getFloorSpace ([]:_) = True
getFloorSpace (_:ws) = getFloorSpace ws

-- | Returns the number of elements in every column.
makeFloorSpace :: World -> [Int]
makeFloorSpace [] = error "Planner.makeFloorSpace: Your world seems to be empty"
makeFloorSpace [w] = [length w]
--makeFloorSpace (w:ws) = makeFloorSpace ws >>= \ls -> return $ 2 * length w : ls
makeFloorSpace (w:ws) = 2 * length w : makeFloorSpace ws

-- | Returns the column a given object exists in.
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
isOver :: Id -> Id -> Bool -> World -> Bool
isOver _ _ _ [] = False
isOver i1 i2 above (w:ws) = case L.elemIndex i1 w of
  Nothing     -> isOver i1 i2 above ws
  Just index1 -> case L.elemIndex i2 w of
    Nothing     -> False
    Just index2 -> let d = index1 - index2
                   in if above then d == 1
                               else d > 0

-- | Checks if the goal is fulfilled in the provided state
check :: Goal -> WorldHolding -> Bool
check goal (w, h) = case goal of
  TakeGoal Flr  -> error "Take floor goal cannot be assessed"
  TakeGoal (Obj i)  -> case h of
    Nothing             -> False
    Just holdingId      -> i == holdingId
  MoveGoal Ontop (Obj i) Flr -> case objPos i w of
    Just height -> height == 0
    Nothing -> False
  MoveGoal rel (Obj i1) (Obj i2) -> case rel of
    Ontop -> isOver i1 i2 True w
    Inside -> isOver i1 i2 True w
    Beside -> isBeside i1 i2 w
    Leftof -> let c1 = fromMaybe (-1) $ clmn 0 i1 w
                  c2 = fromMaybe (-1) $ clmn 0 i2 w
              in (c1 /= (-1) && c2 /= (-1)) && c1 < c2
    Rightof -> let c1 = fromMaybe (-1) $ clmn 0 i1 w
                   c2 = fromMaybe (-1) $ clmn 0 i2 w
               in ((c1 /= (-1) && c2 /= (-1)) && c1 > c2)
    Above -> isOver i1 i2 False w
    Under -> isOver i2 i1 False w
    --_ -> error $ "Not implemented yet: Trying to put " ++ i1 ++ " " ++ show rel ++ " of " ++ i2
  MoveGoal {}      -> error "MoveGoal not fully implemented yet"

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
