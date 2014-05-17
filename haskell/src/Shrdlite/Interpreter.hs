module Shrdlite.Interpreter where
--module Shrdlite.Interpreter (
--    unInterpret
--  , interpretAll
--  , findEntity
--  , locationHolds
--  , searchObjects
--  ) where

import Shrdlite.Common as Common
import Shrdlite.Grammar as Grammar

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Either
import Data.Maybe
import qualified Data.Map as M

type Interpretation = ReaderT State (ErrorT InterpretationError Identity) 
unInterpret :: State -> Interpretation a -> Either InterpretationError a
unInterpret state a = runIdentity $ runErrorT $ runReaderT a state

data InterpretationError = EntityError String
                         | AmbiguityError [Id]
                         | OtherError String
                         deriving (Show, Eq)

isAmbiguityError :: InterpretationError -> Bool
isAmbiguityError (AmbiguityError _) = True
isAmbiguityError _                  = False

isEntityError :: InterpretationError -> Bool
isEntityError (EntityError _) = True
isEntityError _               = False

instance Error InterpretationError where
    noMsg  = OtherError ""
    strMsg = OtherError

-- | Interprets all the provided commands in the given state.
interpretAll :: State -> [Command] -> Either InterpretationError [[Goal]]
interpretAll state cmds = Right validResults
  where results = map (interpret state) cmds
        validResults = rights results
--        invalidResults = lefts results

-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: State -> Command -> Either InterpretationError [Goal]
interpret state cmd = unInterpret state $ interpret' cmd

interpret' :: Command -> Interpretation [Goal]
interpret' (Take ent)     = takeEntity ent
interpret' (Put loc)      = dropAtLocation loc
interpret' (Move ent loc) = moveEntity ent (Just loc)

takeEntity :: Entity -> Interpretation [Goal]
takeEntity ent =
  case ent of
    Floor                    -> throwError $ EntityError "Cannot take floor, ye rascal!"
    BasicEntity q obj        -> searchObjects obj q Nothing >>= makeGoal
    RelativeEntity q obj loc -> searchObjects obj q (Just loc) >>= makeGoal
  where makeGoal found = return $ map (\(i,_) -> TakeGoal (Obj i)) found

dropAtLocation :: Location -> Interpretation [Goal]
dropAtLocation (Relative rel ent) = do
  state <- ask
  let hold = fromJust $ holding state
  case ent of
    Floor                    -> return [MoveGoal rel (Obj hold) Flr]
    BasicEntity q obj        -> searchObjects obj q Nothing    >>= makeGoal hold
    RelativeEntity q obj loc -> searchObjects obj q (Just loc) >>= makeGoal hold
  where makeGoal hold found = return $ map (\(i,_) -> MoveGoal rel (Obj hold) (Obj i)) found

moveEntity :: Entity -> Maybe Location -> Interpretation [Goal]
moveEntity ent (Just (Relative rel ent2)) = do
  movingEntities <- findEntity ent  -- TODO: MUST CONSIDER ALL
  case ent2 of
    Floor -> return $ map (\movingEntity -> MoveGoal rel (Obj movingEntity) Flr) movingEntities
    BasicEntity q obj -> searchObjects obj q Nothing >>= makeGoal movingEntities
    RelativeEntity q obj loc -> searchObjects obj q (Just loc) >>= makeGoal movingEntities
  where makeGoal movingEntities found = 
          if null found
            then throwError $ EntityError "No matching entities."
            else return $ concatMap (\movingEntity -> map (\(i,_) -> MoveGoal rel (Obj movingEntity) (Obj i)) found) movingEntities

-- | Searches the objects map after objects matching the quantifier and location.
-- Returns Left at ambiguity error, and Right otherwise.
searchObjects :: Object -> Quantifier -> Maybe Location -> Interpretation [(Id, Object)]
searchObjects obj quant mloc = do
  state <- ask
  let foundObjects = filter (`exists` state) . filter isMatching $ M.assocs (objects state)
  case mloc of
    Nothing ->
      case quant of
        All -> return foundObjects
        _   -> return foundObjects
    Just loc ->
      case quant of
        All -> return foundObjects
        _   -> filterM (\(i,o) -> locationHolds state (i,o) loc) foundObjects
  where 
    ids state = case holding state of
            Nothing      -> concat $ world state
            Just holdId  -> holdId : concat (world state)
    exists (i,_) state    = i `elem` ids state
    isMatching (_, o) = obj == o

-- | Checks if a location holds for an object in the world.
locationHolds :: State -> (Id, Object) -> Location -> Interpretation Bool
locationHolds state (ide, _) (Relative rel ent) = do
  entityIds <- findEntity ent
  case objPos' of
    Nothing     -> error "Cannot validate location for non-existent objects"
    Just (_, _) -> return $ or $
      case rel of
        Beside  -> map (\eId -> leftof ide eId || rightof ide eId) entityIds
        Leftof  -> map (leftof ide) entityIds
        Rightof -> map (rightof ide) entityIds
        Above   -> map (\eId -> sameColumn ide eId && above ide eId) entityIds
        Ontop   -> map (\eId -> sameColumn ide eId && ontop ide eId) entityIds
        Under   -> map (\eId -> sameColumn ide eId && under ide eId) entityIds
        Inside  -> map (\eId -> sameColumn ide eId && inside ide eId) entityIds
  where
    objPos'          = findObjPos ide (world state)
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

-- | Finds a single entity 
findSingleEntity :: Entity -> Interpretation Id
findSingleEntity entity = do
  found <- findEntity entity
  case found of
    [] -> throwError $ EntityError ("Could not find entity " ++ show entity)
    _  -> return $ head found

-- | Finds all object ids matching the entity in the provided state        
--findEntity :: State -> Entity -> Either [[Id]] [Id]
findEntity :: Entity -> Interpretation [Id]
findEntity ent =
  case ent of
    Floor                    -> return []
    BasicEntity q obj        -> liftM (map fst) $ searchObjects obj q Nothing
    RelativeEntity q obj loc -> liftM (map fst) $ searchObjects obj q (Just loc)

-- | Returns the column index of the object id, if any.
column :: State -> Id -> Maybe [Id]
column state ident = fmap (\pos -> world state !! fst pos) i
  where i = findObjPos ident (world state)
