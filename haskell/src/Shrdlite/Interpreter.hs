module Shrdlite.Interpreter where

import Shrdlite.Grammar
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Data.List

import Shrdlite.Common as Common


-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: State -> Command -> [Goal]
interpret state (Take entity) =
    case entity of
        Floor                    -> error "Cannot take floor, ye rascal!"
        BasicEntity q obj        ->
            case matchingObjects q obj Nothing of
                Right found -> map (\(i,_) -> TakeGoal (Obj i)) found
                Left _     -> error "Ambiguity error - searchObjects returned Left."
        RelativeEntity q obj loc -> 
            case matchingObjects q obj (Just loc) of
                Right found -> map (\(i,_) -> TakeGoal (Obj i)) found
                Left _     -> error "Ambiguity error - searchObjects returned Left."
  where matchingObjects q obj l = searchObjects state obj q l
interpret state goal = undefined


-- | Searches the objects map after objects matching the quantifier and location.
-- Returns Left at ambiguity error, and Right otherwise.
searchObjects :: State -> Object -> Quantifier ->
                 Maybe Location -> Either [[(Id, Object)]] [(Id, Object)]
searchObjects state obj quantifier loc =
    case loc of
        Nothing ->
            case quantifier of
                All -> Right foundObjects
                --Any -> Right $ take 1 foundObjects
                Any -> Right foundObjects
                The -> case length foundObjects of
                           1 -> Right $ take 1 foundObjects
                           _ -> Left $ map (\a -> [a]) foundObjects
        Just location ->
            case quantifier of
                All -> Right foundObjects
                --Any -> Right $ take 1 foundObjects
                Any -> let foundObjects' = filter (\(i, o) -> locationHolds state (i, o) location) foundObjects
                       in Right foundObjects'
                The -> let foundObjects' = filter (\(i, o) -> locationHolds state (i, o) location) foundObjects
                       in case length foundObjects' of
                              1 -> Right $ take 1 foundObjects'
                              0 -> Right []
                              _ -> Left $ map (\a -> [a]) foundObjects'
  where 
        ids = case holding state of
          Nothing   -> concat $ world state
          Just holdId  -> (holdId : (concat $ world state))
        exists (i,_)     = i `elem` ids
        isMatching (_, o) = obj == o
        foundObjects = filter exists . filter isMatching $ M.assocs (objects state)

-- | Checks if a location holds for an object in the world.
locationHolds :: State -> (Id, Object) -> Location -> Bool
locationHolds state (id, obj) (Relative relation entity) =
    case objPos of
        Nothing -> error "Cannot validate location for non-existent objects"
        Just (col, height) -> or $
            case relation of
                Beside  -> map (\eId -> leftof id eId || rightof id eId) $ entityIds
                Leftof  -> map (\eId -> leftof id eId) $ entityIds
                Rightof -> map (\eId -> rightof id eId) $ entityIds
                Above   -> map (\eId -> sameColumn id eId && above id eId) $ entityIds
                Ontop   -> map (\eId -> sameColumn id eId && ontop id eId) $ entityIds
                Under   -> map (\eId -> sameColumn id eId && under id eId) $ entityIds
                Inside  -> map (\eId -> sameColumn id eId && inside id eId) $ entityIds
  where objPos = findObjPos id (world state)
        entityIds = findEntity state entity
        findObjColumn i  = fmap fst . findObjPos i $ world state
        findObjHeight i  = fmap snd . findObjPos i $ world state
        sameColumn i1 i2 = fromMaybe False $ liftM2 elem (return i2) (column state i1)
        above i1 i2      = fromMaybe False $ liftM2 (>) (findObjHeight i1) (findObjHeight i2)
        under i1 i2      = fromMaybe False $ liftM2 (<) (findObjHeight i1) (findObjHeight i2)
        ontop i1 i2      = fromMaybe False $ liftM2 (\a b -> a - b == 1) (findObjHeight i1) (findObjHeight i2)
        inside i1 i2     = ontop i1 i2 && (form' $ (objects state) M.! i2) == Box
        rightof i1 i2    = fromMaybe False $ liftM2 (\a b -> a - b == 1) (findObjColumn i1) (findObjColumn i2)
        leftof i1 i2     = fromMaybe False $ liftM2 (\a b -> a - b == (-1)) (findObjColumn i1) (findObjColumn i2)
        form' (Object _ _ f) = f

-- | Finds all object ids matching the entity in the provided state        
findEntity :: State -> Entity -> [Id]
findEntity state entity =
    case entity of
        Floor -> []
        BasicEntity q obj -> 
            case searchObjects state obj q Nothing of
                Left _ -> error "Ambiguity error."
                Right objs -> map fst objs
        RelativeEntity q obj loc ->
             case searchObjects state obj q (Just loc) of
                Left _ -> error "Ambiguity error."
                Right objs -> map fst objs

-- | Returns the column index of the object id, if any.
column :: State -> Id -> Maybe [Id]
column state id = fmap (\pos -> world state !! fst pos) i
  where i = findObjPos id (world state)
