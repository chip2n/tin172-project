module Shrdlite.Common where

import ShrdliteGrammar
import Data.List (elemIndex)
import qualified Data.Map as M

type Utterance = [String]
type Id = String
type World = [[Id]]
--type Objects = JSObject JSValue
type Objects = M.Map Id Object
data Goal = TakeGoal GoalObject
          | PutGoal Relation GoalObject GoalObject
          deriving (Show, Eq, Ord)
data GoalObject = Flr Int | Obj Id deriving (Show, Eq, Ord)
type Plan = [String]
data State = State { world :: World, holding :: Maybe Id, objects :: Objects }
  deriving (Eq, Ord)

findObjPos :: Id -> World -> Maybe (Int,Int)
findObjPos = findObjPos' 0

findObjPos' :: Int -> Id -> World -> Maybe (Int,Int)
findObjPos' _ _ [] = Nothing
findObjPos' x i (ids:idss) = case elemIndex i ids of
  Nothing -> findObjPos' (x+1) i idss
  Just y  -> Just (x,y)

