module Shrdlite.AmbiguityResolver where

import Shrdlite.Grammar
import Shrdlite.Planner
import qualified Data.Map as M
import Control.Monad
import Debug.Trace
import Data.Maybe
--import Data.List

import Shrdlite.Common as Common

resolveAmbiguity :: State -> [Goal] -> Either String Goal
resolveAmbiguity state goals = 
  case filter (possibleGoal state) goals of
    []     -> Left "No valid goal."
    [g]    -> Right g
    (g:gs) -> Left "Ambiguity error"


possibleGoal :: State -> Goal -> Bool
possibleGoal _     (TakeGoal Flr) = False
possibleGoal _     (TakeGoal obj) = True
possibleGoal _     (MoveGoal _        Flr      _)        = False
possibleGoal _     (MoveGoal _        _      Flr)        = True
possibleGoal state (MoveGoal relation (Obj i1) (Obj i2)) = validateAllLaws relation o1 o2
  where o1 = fromJust $ M.lookup i1 $ objects state
        o2 = fromJust $ M.lookup i2 $ objects state
        
--data Goal = TakeGoal GoalObject
--          | PutGoal Relation GoalObject GoalObject
--          deriving (Show, Eq, Ord)
