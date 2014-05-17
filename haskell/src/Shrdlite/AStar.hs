module Shrdlite.AStar where

import Shrdlite.Common
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding(..))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

-- | The current state of the algoritm. Closed nodes are visited, open is a
-- queue of nodes to visit, pathCost keeps track of the cost of getting to each
-- node and parent is a map from each node to its parent or previous node.
data AStarState c = AStarState { closed   :: !(S.Set WorldHolding)
                               , open     :: !(Q.PSQ WorldHolding c)
                               , pathCost :: !(M.Map WorldHolding c)
                               , parent   :: !(M.Map WorldHolding WorldHolding)}

-- | Find the best path from the initial node to (one of) the goal state(s).
-- Gives this in the form of a 'Plan'.
aStar :: (WorldHolding -> S.Set WorldHolding) -- ^ Graph to search through
        -> (WorldHolding -> Int)              -- ^ Heuristic distance to the goal
        -> (WorldHolding -> Bool)             -- ^ Goal check function
        -> WorldHolding                       -- ^ Starting point
        -> Maybe Plan                         -- ^ Resulting path if it exists
aStar graph heur check start =
    aStar' initState graph heur check
  where initState = AStarState {closed = S.empty,
                                open = Q.singleton start (heur start),
                                pathCost = M.singleton start 0,
                                parent = M.empty}

-- | Much like 'aStar', but also takes an initial state
aStar' :: AStarState Int                        -- ^ Initial state
        -> (WorldHolding -> S.Set WorldHolding) -- ^ Graph to search through
        -> (WorldHolding -> Int)                -- ^ Heuristic distance to the goal
        -> (WorldHolding -> Bool)               -- ^ Goal check function
        -> Maybe Plan                           -- ^ Resulting path if it exists
aStar' s g h c = case Q.minView $ open s of -- take best open node
    Just (node :-> _, open') -> -- the chosen node and updated open queue
        if c node -- the goal is found
        then Just $ resultList node $ parent state' -- find result here
        else aStar' state' g h c -- run astar' with updated state
      where
        state' = S.fold (updateState h node) (s {closed=closed', open=open'}) newNodes
        closed' = S.insert node $ closed s -- add node to the closed list
        newNodes = S.difference (g node) closed' -- adjacent nodes not visited
    Nothing -> Nothing -- open queue empty, no result found

updateState :: (Ord c, Num c)
            => (WorldHolding -> c)  -- heuristic function
            -> WorldHolding         -- previous node
            -> WorldHolding         -- current node
            -> AStarState c         -- state to update
            -> AStarState c         -- new state
updateState h previous x state = if pCost > oldCost
    then state -- do nothing is the path to get to the state is longer
    else state {open=open', pathCost=pathCost', parent=parent'}
  where
    pCost = 1 + fromMaybe 0 (M.lookup previous $ pathCost state) -- this cost
    oldCost = fromMaybe pCost (M.lookup x $ pathCost state)
    -- update parent if the path cost is shorter
    parent' = M.insert x previous $ parent state
    -- add path lengths to pathCost (1+pathCost for previous) (M.insertWith' min)
    pathCost' = M.insert x pCost $ pathCost state
    -- add new nodes with cost+heuristic to open queue (Q.insertWith min)
    open' = Q.insert x (pCost + h x) $ open state

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
