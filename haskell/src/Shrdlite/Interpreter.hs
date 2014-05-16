module Shrdlite.Interpreter (
    interpretAll
  , findEntity
  , locationHolds
  , searchObjects
  ) where

import Shrdlite.Common as Common
import Shrdlite.Grammar as Grammar

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Data.Either
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M


type Interpretation = ErrorT InterpretationError Identity
unInterpret :: Interpretation a -> Either InterpretationError a
unInterpret a = runIdentity $ runErrorT $ a

data InterpretationError = EntityError String | AmbiguityError [Id] | OtherError String
  deriving (Show, Eq)

isEntityError :: InterpretationError -> Bool
isEntityError (EntityError _) = True
isEntityError _               = False

isAmbiguityError :: InterpretationError -> Bool
isAmbiguityError (AmbiguityError _) = True
isAmbiguityError _                  = False

isOtherError :: InterpretationError -> Bool
isOtherError (OtherError _) = True
isOtherError _              = False

instance Error InterpretationError where
    noMsg  = OtherError ""
    strMsg = OtherError

interpretAll :: State -> [Command] -> Either InterpretationError [[Goal]]
interpretAll state cmds =
  case validInterpretations of
    []     -> error "No valid interpretations"
    [a]    -> Right [a]
    (a:as) -> Left $ AmbiguityError undefined
  where interpretations = map (interpret state) cmds
        validInterpretations = rights interpretations
        ambiguityInterpretations = filter (\i -> isAmbiguityError i) (lefts interpretations)

-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: State -> Command -> Either InterpretationError [Goal]
interpret state cmd = unInterpret $ interpret' cmd
  where interpret' (Take ent)     = takeEntity state ent
        interpret' (Put loc)      = dropAtLocation state loc
        interpret' (Move ent loc) = moveEntity state ent (Just loc)

takeEntity :: State -> Entity -> Interpretation [Goal]
takeEntity state ent =
  case ent of
    Floor                    -> throwError $ EntityError "Cannot take floor, ye rascal!"
    BasicEntity q obj        ->
      case searchObjects state obj q Nothing of
        Right found -> return $     map (\(i,_) -> TakeGoal (Obj i)) found
        Left _      -> throwError $ AmbiguityError undefined
    RelativeEntity q obj loc -> 
      case searchObjects state obj q (Just loc) of
        Right found -> return $ map (\(i,_) -> TakeGoal (Obj i)) found
        Left _      -> throwError $ AmbiguityError undefined

dropAtLocation :: State -> Location -> Interpretation [Goal]
dropAtLocation state (Relative rel ent) =
  case ent of
    Floor                    -> return $ [MoveGoal rel (Obj hold) Flr]
    BasicEntity q obj        ->
      case searchObjects state obj q Nothing of
        Right found -> return $ map (\(i,_) -> MoveGoal rel (Obj hold) (Obj i)) found
        Left _      -> throwError $ AmbiguityError undefined
    RelativeEntity q obj loc -> 
      case searchObjects state obj q (Just loc) of
        Right found -> return $ map (\(i,_) -> MoveGoal rel (Obj hold) (Obj i)) found
        Left _      -> throwError $ AmbiguityError undefined
  where hold = fromJust $ holding state

moveEntity :: State -> Entity -> Maybe Location -> Interpretation [Goal]
moveEntity state ent (Just (Relative rel ent2)) =
  case ent2 of
    BasicEntity q obj ->
      case searchObjects state obj q Nothing of
        Right found -> return $ map (\(i,_) -> MoveGoal rel (Obj movingEntity) (Obj i)) found
        Left _      -> throwError $ AmbiguityError undefined
    RelativeEntity q obj loc -> 
      case searchObjects state obj q (Just loc) of
        Right found -> return $ map (\(i,_) -> MoveGoal rel (Obj movingEntity) (Obj i)) found
        Left _      -> throwError $ AmbiguityError undefined
    Floor -> return $ [MoveGoal rel (Obj movingEntity) Flr]
  where Right movingEntity = findSingleEntity state ent --TODO: Handle ambiguity

-- | Searches the objects map after objects matching the quantifier and location.
-- Returns Left at ambiguity error, and Right otherwise.
searchObjects :: State -> Object -> Quantifier ->
                 Maybe Location -> Either [[(Id, Object)]] [(Id, Object)]
searchObjects state obj quant mloc =
  case mloc of
    Nothing ->
      case quant of
        All -> Right foundObjects
        Any -> Right foundObjects
        The -> case length foundObjects of
          1 -> Right $ take 1 foundObjects
          _ -> Left $ map (: []) foundObjects
    Just loc ->
      case quant of
        All -> Right foundObjects
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

    -- TODO: Handle no objects, and handle ambiguity
findSingleEntity :: State -> Entity -> Either Ambiguity Id
findSingleEntity state entity =
  case findEntity state entity of
    Left _ -> error $ "Ambiguity error"
    Right a -> Right $ head a

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
