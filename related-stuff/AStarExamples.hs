module AStarExample where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Data.Graph.AStar

type PuzzleState = [[Int]]

-- Solves the puzzle using aStar given an initial state
slidingPuzzleSolver :: PuzzleState -> Maybe [PuzzleState]
slidingPuzzleSolver = aStar puzzleGraph dist misplaced puzzleGoal

-- Takes an initial PuzzleState and gets the neighbours of that state,
-- i.e. the possible moves that can follow. This is the representation
-- of the graph.
puzzleGraph :: PuzzleState -> S.Set PuzzleState
puzzleGraph ps = S.fromList $ map fromJust $ filter isJust [uPS,rPS,dPS,lPS]
  where
    uPS = nextPS ps zeroPos (id,flip (-) 1)
    rPS = nextPS ps zeroPos ((+1),id)
    dPS = nextPS ps zeroPos (id,(+1))
    lPS = nextPS ps zeroPos (flip (-) 1,id)
    zeroPos = head [(fromJust mX,y) |
      (mX,y)<-zip (map (L.elemIndex 0) ps) ([0..]::[Int]),
      isJust mX]

-- The distance between two neitgboring nodes, i.e. two states.
-- This is always 1, since we have the same cost for any move.
dist :: PuzzleState -> PuzzleState -> Int
dist = const.const 1

-- Heuristics counting the number of misplaced tiles
misplaced :: PuzzleState -> Int
misplaced ps = diffCount (concat ps) (concat solution)
  where
    diffCount [] [] = 0
    diffCount (x:xs) (y:ys) = if x /= y && x /= 0
                              then 1 + diffCount xs ys
                              else diffCount xs ys

-- The goal is reached if there are no misplaced tiles
puzzleGoal :: PuzzleState -> Bool
puzzleGoal ps = misplaced ps == 0

-- a puzzle that sould be simple to solve
simplepuzzle :: PuzzleState
simplepuzzle = [[1,2,3],[4,5,6],[7,0,8]]

-- the solution to the puzzle
solution :: PuzzleState
solution = [[1,2,3],[4,5,6],[7,8,0]]

-- Helper functions

-- Makes an adjacent PuzzleState given the position of the empty tile
-- and two functions that describe where it is to be moved next
nextPS :: PuzzleState -> (Int,Int) -> (Int->Int,Int->Int) -> Maybe PuzzleState
nextPS ps (x,y) (xf,yf)
  | x2 < 0 || x2 > 2 || y2 < 0 || y2 > 2  = Nothing
  | otherwise                             = Just $ moveTile ps (x2,y2) (x,y)
    where
      (x2,y2) = (xf x,yf y)

-- The first pair is the position of the empty tile and the
-- and the second one is the one to be moved, so the value
-- in the first position is ignored
moveTile :: PuzzleState -> (Int,Int) -> (Int,Int) -> PuzzleState
moveTile ps (x,y) (x2,y2) = ps''
  where
    num = ps !! y !! x
    row1 = (ps !! y2) !!= (x2,num)
    ps' = ps !!= (y2,row1)
    row2 = (ps' !! y) !!= (x,0)
    ps'' = ps' !!= (y,row2)

-- updates a list, placing element x at position p in list xs
-- indexes <= the length of the list adds the element in front
-- and indexes > the length adds it to the end
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (p,x) = take p xs ++ [x] ++ drop (p+1) xs

-- Simple usage of aStar, on a graph of Ints

graphEx :: Int -> S.Set Int
graphEx 0 = S.insert 1 (S.singleton 2)
graphEx 1 = S.singleton 3
graphEx 2 = S.singleton 4
graphEx 3 = S.singleton 4
graphEx 4 = S.empty

distEx :: Int -> Int -> Int
distEx = const.const 1

hEx :: Int -> Int
hEx = const 0

goalEx :: Int -> Bool
goalEx 4 = True
goalEx _ = False

aStarEx :: Maybe [Int]
aStarEx = aStar graphEx distEx hEx goalEx 0
