module Shrdlite.AStar where

import Shrdlite.Common
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding(..))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

data AStarState c = AStarState { closed   :: S.Set WorldHolding
                                             , open     :: !(Q.PSQ WorldHolding c)
                                             , pathCost :: !(M.Map WorldHolding c)
                                             , parent   :: !(M.Map WorldHolding WorldHolding)}

aStar :: (Ord c, Num c)
        => Objects        -- Objects in the world
        -> (WorldHolding -> S.Set WorldHolding) -- Graph to search through
        -> (WorldHolding -> c)       -- Heuristic distance to the goal
        -> (WorldHolding -> Bool)    -- Goal check function
        -> WorldHolding              -- Starting point
        -> Maybe Plan -- Resulting path if it exists
aStar o graph heur check start =
  aStar' o initState graph heur check
    where initState = AStarState {closed = S.empty,
                                  open = Q.singleton start (heur start),
                                  pathCost = M.singleton start 0,
                                  parent = M.empty}

aStar' :: (Ord c, Num c)
        => Objects        -- Objects in the world
        -> AStarState c -- Initial state
        -> (WorldHolding -> S.Set WorldHolding) -- Graph to search through
        -> (WorldHolding -> c)       -- Heuristic distance to the goal
        -> (WorldHolding -> Bool)    -- Goal check function
        -> Maybe Plan -- Resulting path if it exists
aStar' o s g h c = case Q.minView $ open s of -- take best open node
  Just (node :-> _, newOpen) -> let -- the chosen node and updated open queue
    newClosed = S.insert node $ closed s -- add node to the closed list
    adjacent = g node
    newNodes = S.difference adjacent newClosed -- adjacent nodes not visited
    state' = S.fold updateState (s {closed=newClosed, open=newOpen}) newNodes
    updateState x state = newState
      where
        pCost = 1 + fromMaybe 0 (M.lookup node $ pathCost state) -- this cost
        oldCost = fromMaybe pCost (M.lookup x $ pathCost state)
        newState = if pCost > oldCost
          then state
          else state {open=open', pathCost=pathCost', parent=parent'}
        -- update parent if the path cost is shorter
        parent' = M.insert x node $ parent state
        -- add path lengths to pathCost (1+pathCost for node) (M.insertWith' min)
        pathCost' = M.insert x pCost $ pathCost state
        -- add new nodes with cost+heuristic to open queue (Q.insertWith min)
        open' = Q.insert x (pCost + h x) $ open state
    in if c node -- the goal is found
      then Just $ resultList node $ parent state' -- find result here
      else aStar' o state' g h c -- run astar' with updated state
  Nothing -> Nothing -- open queue empty, no result found

resultList :: WorldHolding -> M.Map WorldHolding WorldHolding -> Plan
resultList x@(w, _) xm = case M.lookup x xm of
  Nothing         -> []
  Just x'@(w', _) -> resultList x' xm ++ stateTrans w' w 0

stateTrans :: World -> World -> Int -> Plan
stateTrans [] [] _ = error "stateTransition: no changes"
stateTrans (c1:c1s) (c2:c2s) col = case compare (length c1) (length c2) of
  LT -> ["drop " ++ show col]
  GT -> ["pick " ++ show col]
  EQ -> stateTrans c1s c2s (col + 1)
