module Shrdlite.AmbiguityResolver where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Either
import Data.Maybe
import qualified Data.Map as M

import Shrdlite.Common as Common

import Debug.Trace

type Ambiguity = ReaderT State (ErrorT AmbiguityError Identity)

data AmbiguityError = AmbiguityError [Goal] String
                    | OtherError String
                    deriving (Show, Eq, Ord)

unAmbiguity :: State -> Ambiguity a -> Either AmbiguityError a
unAmbiguity state a = runIdentity $ runErrorT $ runReaderT a state

instance Error AmbiguityError where
    noMsg  = AmbiguityError [] ""
    strMsg = AmbiguityError []

resolveAmbiguity :: State -> [[Goal]] -> Either AmbiguityError Goal
resolveAmbiguity state goals = 
  case validResults of
    [] -> case length results of
            0 -> Left $ OtherError "No valid goals."
            1 -> head results
            _ -> minimum results
    _  -> case trace (show $ length validResults) (length validResults) of
            0 -> Left $ OtherError "No valid goals."
            1 -> Right $ head validResults
            _ -> Left $ AmbiguityError validResults "INSERT AMBIGUITY QUESTION HERE" -- TODO
  where results = map (resolveAmbiguity' state) goals :: [Either AmbiguityError Goal]
        validResults = rights results

resolveAmbiguity' :: State -> [Goal] -> Either AmbiguityError Goal
resolveAmbiguity' state goals = unAmbiguity state (computation goals)

computation :: [Goal] -> Ambiguity Goal
computation goals = do
  validated <- validGoals goals
  case validated of
    [] -> throwError $ OtherError "No valid goal."
    gs -> pickOne gs

-- | Picks one goal if possible.
-- TODO: questions
pickOne :: [Goal] -> Ambiguity Goal
pickOne [] = throwError $ OtherError "No valid goal."
pickOne [g] = return g
pickOne gs = throwError $ AmbiguityError gs "INSERT AMBIGUITY QUESTION HERE" -- TODO



-- | Gets all the possible goals
validGoals :: [Goal] -> Ambiguity [Goal]
validGoals = filterM possibleGoal

-- | Checks if the provided goal is possible.
possibleGoal :: Goal -> Ambiguity Bool
possibleGoal goal = do
  state <- ask
  return $ case goal of
    (TakeGoal Flr) -> False
    (TakeGoal _)   -> True -- Planner handles this case
    (MoveGoal _ Flr _)   -> False
    (MoveGoal _ _   Flr) -> True
    (MoveGoal relation (Obj _ i1) (Obj _ i2)) ->
      let o1 = fromJust $ M.lookup i1 $ objects state
          o2 = fromJust $ M.lookup i2 $ objects state
      in validateAllLaws relation o1 o2
