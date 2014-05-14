module Shrdlite.Interpreter where

import Shrdlite.Grammar
import Shrdlite.Planner
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
--import Data.List

import Shrdlite.Common as Common


-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: State -> Command -> [Goal]
interpret state (Take ent) =
  case ent of
    Floor                    -> error "Cannot take floor, ye rascal!"
    BasicEntity q obj        ->
      case matchingObjects q obj Nothing of
        Right found -> map (\(i,_) -> TakeGoal (Obj i)) found
        Left _      -> error "Ambiguity error - searchObjects returned Left."
    RelativeEntity q obj loc -> 
      case matchingObjects q obj (Just loc) of
        Right found -> map (\(i,_) -> TakeGoal (Obj i)) found
        Left _      -> error "Ambiguity error - searchObjects returned Left."
  where matchingObjects q obj = searchObjects state obj q 
interpret state (Put (Relative rel ent)) = 
  case ent of
    BasicEntity q obj ->
      case matchingObjects q obj Nothing of
        Right found -> map (\(i,_) -> PutGoal rel (Obj hold) (Obj i)) found
        Left _      -> error "Ambiguity error - searchObject returned Left."
    RelativeEntity q obj loc -> 
      case matchingObjects q obj (Just loc) of
        Right found -> map (\(i,_) -> PutGoal rel (Obj hold) (Obj i)) found
        Left _      -> error "Ambiguity error - searchObjects returned Left."
    Floor -> case holding state of
      Just ident -> return (PutGoal Ontop (Obj ident) Flr)
      Nothing    -> error "You're not holding anything."
  where hold = fromJust (holding state)
        matchingObjects q obj = searchObjects state obj q
interpret state (Move ent loc) = goals ++ interpret newState (Put loc)
   where newState = state{world = newWorld, holding = newHolding}
         (newWorld, newHolding) = simulatePlan (world state) (holding state) plan
         plan = solve (world state) (holding state) (objects state) goals
         goals = interpret state (Take ent)

--PutGoal Relation GoalObject GoalObject


-- | Searches the objects map after objects matching the quantifier and location.
-- Returns Left at ambiguity error, and Right otherwise.
searchObjects :: State -> Object -> Quantifier ->
                 Maybe Location -> Either [[(Id, Object)]] [(Id, Object)]
searchObjects state obj quant mloc =
  case mloc of
    Nothing ->
      case quant of
        All -> Right foundObjects
        --Any -> Right $ take 1 foundObjects
        Any -> Right foundObjects
        The -> case length foundObjects of
          1 -> Right $ take 1 foundObjects
          _ -> Left $ map (: []) foundObjects
    Just loc ->
      case quant of
        All -> Right foundObjects
        --Any -> Right $ take 1 foundObjects
        Any -> let foundObjects' = filter (\(i, o) ->
                     locationHolds state (i, o) loc) foundObjects
               in Right foundObjects'
        The -> let foundObjects' = filter (\(i, o) ->
                     locationHolds state (i, o) loc) foundObjects
               in case length foundObjects' of
                 1 -> Right $ take 1 foundObjects'
                 0 -> Right []
                 _ -> Left $ map (: []) foundObjects'
  where 
    ids = case holding state of
            Nothing      -> concat $ world state
            Just holdId  -> holdId : concat (world state)
    exists (i,_)     = i `elem` ids
    isMatching (_, o) = obj == o
    foundObjects = filter exists . filter isMatching $ M.assocs (objects state)

-- | Checks if a location holds for an object in the world.
locationHolds :: State -> (Id, Object) -> Location -> Bool
locationHolds state (ide, _) (Relative rel ent) =
  case objPos' of
    Nothing     -> error "Cannot validate location for non-existent objects"
    Just (_, _) -> or $
      case rel of
        Beside  -> map (\eId -> leftof ide eId || rightof ide eId) entityIds
        Leftof  -> map (leftof ide) entityIds
        Rightof -> map (rightof ide) entityIds
        Above   -> map (\eId -> sameColumn ide eId && above ide eId) entityIds
        Ontop   -> map (\eId -> sameColumn ide eId && ontop ide eId) entityIds
        Under   -> map (\eId -> sameColumn ide eId && under ide eId) entityIds
        Inside  -> map (\eId -> sameColumn ide eId && inside ide eId) entityIds
  where
    objPos'           = findObjPos ide (world state)
    entityIds        = case findEntity state ent of
                         Right objs -> objs
                         Left _     -> []
    findObjColumn i  = fmap fst . findObjPos i $ world state
    findObjHeight i  = fmap snd . findObjPos i $ world state
    sameColumn i1 i2 = fromMaybe False $ liftM (elem i2) (column state i1)
    above i1 i2      = fromMaybe False
                         $ liftM2 (>) (findObjHeight i1) (findObjHeight i2)
    under i1 i2      = fromMaybe False
                         $ liftM2 (<) (findObjHeight i1) (findObjHeight i2)
    ontop i1 i2      = fromMaybe False $ liftM2 (\a b -> a - b == 1)
                                      (findObjHeight i1) (findObjHeight i2)
    inside i1 i2     = ontop i1 i2 && frm (objects state M.! i2) == Box
    rightof i1 i2    = fromMaybe False $ liftM2 (\a b -> a - b == 1)
                                      (findObjColumn i1) (findObjColumn i2)
    leftof i1 i2     = fromMaybe False $ liftM2 (\a b -> a - b == (-1))
                                      (findObjColumn i1) (findObjColumn i2)
    frm (Object _ _ f) = f

-- | Finds all object ids matching the entity in the provided state        
findEntity :: State -> Entity -> Either [[Id]] [Id]
findEntity state ent =
  case ent of
    Floor -> Right []
    BasicEntity q obj -> 
      case searchObjects state obj q Nothing of
        Left objs -> Left $ map (map fst) objs
        Right objs -> Right $ map fst objs
    RelativeEntity q obj loc ->
      case searchObjects state obj q (Just loc) of
        Left objs -> Left $ map (map fst) objs
        Right objs -> Right $ map fst objs

-- | Returns the column index of the object id, if any.
column :: State -> Id -> Maybe [Id]
column state ident = fmap (\pos -> world state !! fst pos) i
  where i = findObjPos ident (world state)
