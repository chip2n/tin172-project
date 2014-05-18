module Shrdlite.AmbiguityResolver where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Either
import Data.Maybe
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S

import Shrdlite.Grammar
import Shrdlite.Common as Common

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
    _  -> case length validResults of
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
-- pickOne gs = throwError $ AmbiguityError gs "INSERT AMBIGUITY QUESTION HERE" -- TODO
pickOne (g:gs) = do
  foldM_ sameGoalType g gs
  let (leftIds, rightIds) = foldr addObjects (S.empty, S.empty) (g:gs)
      (leftThe, rightThe) = foldr findThe (False, False) (g:gs)
  findAmbiguity leftThe leftIds (g:gs)
  findAmbiguity rightThe rightIds (g:gs)
  return g -- TODO: any goal will do, so take the first one

-- | Throws an @OtherError@ if the goals do not have the same type
sameGoalType :: Goal -> Goal -> Ambiguity Goal
sameGoalType (TakeGoal _) g@(TakeGoal _) = return g
sameGoalType (MoveGoal {}) g@(MoveGoal {}) = return g
sameGoalType _  _ = throwError $ OtherError "Different kinds of goals found."

-- | Adds the object @Ids@ from the @Goal@ to the @Sets@
addObjects :: Goal -> (S.Set Id, S.Set Id) -> (S.Set Id, S.Set Id)
addObjects g (l, r) = case g of
  (TakeGoal Flr) -> (l, r)
  (TakeGoal (Obj _ i)) -> (S.insert i l,r)
  (MoveGoal _ Flr Flr) -> (l, r)
  (MoveGoal _ Flr (Obj _ i2)) -> (l, S.insert i2 r)
  (MoveGoal _ (Obj _ i1) Flr) -> (S.insert i1 l, r)
  (MoveGoal _ (Obj _ i1) (Obj _ i2)) -> (S.insert i1 l, S.insert i2 r)

-- | Check if there is a @The@ quantifier in either object in the @Goal@ or
-- if the @Bool@ tuple has @True@ in the corresponding field
findThe :: Goal -> (Bool, Bool) -> (Bool, Bool)
findThe g (l, r) = case g of
  (TakeGoal Flr) -> (l, r)
  (TakeGoal (Obj q _)) -> (q == The || l,r)
  (MoveGoal _ Flr Flr) -> (l, r)
  (MoveGoal _ Flr (Obj q _)) -> (l, q == The || r)
  (MoveGoal _ (Obj q _) Flr) -> (q == The || l, r)
  (MoveGoal _ (Obj q1 _) (Obj q2 _)) -> (q1 == The || l, q2 == The || r)

-- | If the first argument is @False@, there is no ambiguity. Otherwise, there
-- is an amibiguity if there are serveral @Ids@ in the set, and then an
-- @AmbiguityError@ is thrown
findAmbiguity :: Bool -> S.Set Id -> [Goal] -> Ambiguity ()
findAmbiguity False _ _ = return ()
findAmbiguity True s gs = if S.size s == 1
  then return ()
  else do
    state <- ask
    let os = objects state
    throwError $ AmbiguityError gs $
      "Ambiguity: you could mean " ++ showObjects os s

-- | Finds the objects in the set and shows them in an appropriate manner
showObjects :: Objects -> S.Set Id -> String
showObjects os s = foldr1 showOr $ map (show . (os !)) $ S.elems s
  where
    showOr x y = x ++ " or " ++ y

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
