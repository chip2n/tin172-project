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

-- | The Interpretation monad. Errors in the interpretation are propagated.
type Interpretation = ReaderT State (ErrorT InterpretationError Identity) 

-- | Computes the interpretation with the given world state.
unInterpret :: State -> Interpretation a -> Either InterpretationError a
unInterpret state a = runIdentity $ runErrorT $ runReaderT a state

-- | Data structure for different errors that can show up during the
-- interpretation process.
data InterpretationError = EntityError String
                         | AmbiguityError [Id]
                         | OtherError String
                         deriving (Show, Eq)

-- | For the ErrorT monad.
instance Error InterpretationError where
    noMsg  = OtherError ""
    strMsg = OtherError

-- | Checks wether the provided error is an AmbiguityError.
isAmbiguityError :: InterpretationError -> Bool
isAmbiguityError (AmbiguityError _) = True
isAmbiguityError _                  = False

-- | Checks wether the provided error is an EntityError.
isEntityError :: InterpretationError -> Bool
isEntityError (EntityError _) = True
isEntityError _               = False

-- | Interprets all the provided commands in the given state.
interpretAll :: State -> [Command] -> Either InterpretationError [[Goal]]
interpretAll state cmds = Right validResults
  where results = map (interpret state) cmds
        validResults = rights results

-- | Converts a parse tree into a PDDL representation of the final
-- goal of the command
interpret :: State -> Command -> Either InterpretationError [Goal]
interpret state cmd = unInterpret state $ interpret' cmd

-- | Takes a command and tries to interpret it into a PDDL representation.
interpret' :: Command -> Interpretation [Goal]
interpret' (Take ent)     = takeEntity ent
interpret' (Put loc)      = dropAtLocation loc
interpret' (Move ent loc) = moveEntity ent (Just loc)

-- | Generates a list of TakeGoals with the provided Entity.
takeEntity :: Entity -> Interpretation [Goal]
takeEntity ent =
  case ent of
    Floor                    -> throwError $ EntityError "Cannot take floor, ye rascal!"
    BasicEntity q obj        -> searchObjects obj q Nothing >>= makeGoal
    RelativeEntity q obj loc -> searchObjects obj q (Just loc) >>= makeGoal
  where makeGoal found = return $ map (\(i,_) -> TakeGoal (Obj i)) found

-- | Generates a list of MoveGoals with the provided Location, using the
-- currently held item as the entity to move.
dropAtLocation :: Location -> Interpretation [Goal]
dropAtLocation (Relative rel ent) = do
  state <- ask
  let hold = fromJust $ holding state
  case ent of
    Floor                    -> return [MoveGoal rel (Obj hold) Flr]
    BasicEntity q obj        -> searchObjects obj q Nothing    >>= makeGoal hold
    RelativeEntity q obj loc -> searchObjects obj q (Just loc) >>= makeGoal hold
  where makeGoal hold found = return $ map (\(i,_) -> MoveGoal rel (Obj hold) (Obj i)) found

-- | Generates a list of MoveGoals with the provided Entity and Location.
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
            else return $ concatMap (\movingEntity -> map (\(i,_) ->
                MoveGoal rel (Obj movingEntity) (Obj i)) found) movingEntities

-- | Searches the world state after objects matching the provided quantifier and
-- location.
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
        _   -> filterM (\(i,o) -> locationHolds (i,o) loc) foundObjects
  where 
    ids state = case holding state of
            Nothing      -> concat $ world state
            Just holdId  -> holdId : concat (world state)
    exists (i,_) state    = i `elem` ids state
    isMatching (_, o) = obj == o

-- | Checks if a location holds for an object in the world.
locationHolds :: (Id, Object) -> Location -> Interpretation Bool
locationHolds (ide, _) (Relative rel ent) = do
  state <- ask
  let objPos' = findObjPos ide (world state)
  entityIds <- findEntity ent
  case objPos' of
    Nothing     -> throwError $ EntityError "Could not find a valid entity."
    Just (_, _) -> anyValid $
      case rel of
        Beside  -> anyC [rightof] ide entityIds
        Leftof  -> allC [leftof] ide entityIds
        Rightof -> allC [rightof] ide entityIds
        Above   -> allC [sameColumn, above] ide entityIds
        Ontop   -> allC [sameColumn, ontop] ide entityIds
        Under   -> allC [sameColumn, under] ide entityIds
        Inside  -> allC [sameColumn, inside] ide entityIds

  where anyValid is = liftM or is
        allValid is = liftM and is
        allC fs i1 is = sequence $ map (\i2 -> allValid $ sequence $ map (\f -> f i1 i2) fs) is
        anyC fs i1 is = sequence $ map (\i2 -> anyValid $ sequence $ map (\f -> f i1 i2) fs) is

-- | Finds the column index of the object whose Id matches the provided one.
findObjColumn :: Id -> Interpretation Int
findObjColumn i = do
  state <- ask
  case findObjPos i (world state) of
    Nothing    -> throwError $ EntityError $
                    "Could not find column for entity with id" ++ show i
    Just (c,_) -> return c

-- | Finds the height index of the object whose Id matches the provided one.
findObjHeight :: Id -> Interpretation Int
findObjHeight i = do
  state <- ask
  case findObjPos i (world state) of
    Nothing    -> throwError $ EntityError $
                    "Could not find column for entity with id" ++ show i
    Just (_,h) -> return h

-- | Checks if two objects are in the same column.
sameColumn :: Id -> Id -> Interpretation Bool
sameColumn i1 i2 = do
  state <- ask
  return $ fromMaybe False $ liftM (elem i2) (column state i1)

-- | Checks if the first object is inside the second.
inside :: Id -> Id -> Interpretation Bool
inside i1 i2 = do
  state <- ask
  ontopResult <- ontop i1 i2
  return $ ontopResult && getForm (objects state M.! i2) == Box

-- | Checks if the first object is on top of the second.
ontop :: Id -> Id -> Interpretation Bool
ontop i1 i2 = do
  state <- ask
  liftM2 (\a b -> a - b == 1) (findObjHeight i1) (findObjHeight i2)

-- | Checks if the first object is above the second.
above :: Id -> Id -> Interpretation Bool
above i1 i2 = do
  state <- ask
  liftM2 (>) (findObjHeight i1) (findObjHeight i2)

-- | Checks if the first object is under the second.
under :: Id -> Id -> Interpretation Bool
under i1 i2 = do
  state <- ask
  liftM2 (<) (findObjHeight i1) (findObjHeight i2)

-- | Checks if the first object is to the left of the second.
leftof :: Id -> Id -> Interpretation Bool
leftof i1 i2 = do
  state <- ask
  liftM2 (\a b -> a - b == (-1)) (findObjColumn i1) (findObjColumn i2)

-- | Checks if the first object is to the right of the second.
rightof :: Id -> Id -> Interpretation Bool
rightof i1 i2 = do
  state <- ask
  liftM2 (\a b -> a - b == 1) (findObjColumn i1) (findObjColumn i2)

-- | Finds a single entity 
findSingleEntity :: Entity -> Interpretation Id
findSingleEntity entity = do
  found <- findEntity entity
  case found of
    [] -> throwError $ EntityError ("Could not find entity " ++ show entity)
    _  -> return $ head found

-- | Finds all object ids matching the entity in the provided state        
findEntity :: Entity -> Interpretation [Id]
findEntity ent = case ent of
    Floor                    -> return []
    BasicEntity q obj        -> liftM (map fst) $ searchObjects obj q Nothing
    RelativeEntity q obj loc -> liftM (map fst) $ searchObjects obj q (Just loc)

-- | Returns the column index of the object id, if any.
column :: State -> Id -> Maybe [Id]
column state ident = fmap (\pos -> world state !! fst pos) i
  where i = findObjPos ident (world state)
