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
import Debug.Trace
import Text.JSON
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

instance Error InterpretationError where
    noMsg  = OtherError ""
    strMsg = OtherError

-- | Interprets all the provided commands in the given state.
interpretAll :: State -> [Command] -> Either InterpretationError [[Goal]]
interpretAll state cmds = trace ("results: " ++ show results) Right validResults
  where results = map (interpret state) cmds
        validResults = rights results
        invalidResults = lefts results
--unInterpret state $ mapM interpret' cmds

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
    BasicEntity q obj        -> do
      found <- searchObjects obj q Nothing
      makeGoal found
    RelativeEntity q obj loc -> do
      found <- searchObjects obj q (Just loc)
      makeGoal found
  where makeGoal found = return $ map (\(i,_) -> TakeGoal (Obj i)) found

dropAtLocation :: Location -> Interpretation [Goal]
dropAtLocation (Relative rel ent) = do
  state <- ask
  let hold = fromJust $ holding state
  case ent of
    Floor                    -> return $ [MoveGoal rel (Obj hold) Flr]
    BasicEntity q obj        -> do
      found <- searchObjects obj q Nothing
      makeGoal found hold
    RelativeEntity q obj loc -> do
      found <- searchObjects obj q (Just loc)
      makeGoal found hold
  where makeGoal found hold = return $ map (\(i,_) -> MoveGoal rel (Obj hold) (Obj i)) found

moveEntity :: Entity -> Maybe Location -> Interpretation [Goal]
moveEntity ent (Just (Relative rel ent2)) = do
  movingEntity <- findSingleEntity ent
  case ent2 of
    BasicEntity q obj -> do
      found <- searchObjects obj q Nothing
      makeGoal found movingEntity
    RelativeEntity q obj loc -> do
      found <- searchObjects obj q (Just loc)
      makeGoal found movingEntity
    Floor -> return $ [MoveGoal rel (Obj movingEntity) Flr]
  where makeGoal found movingEntity = return $ map (\(i,_) -> MoveGoal rel (Obj movingEntity) (Obj i)) found

-- | Searches the objects map after objects matching the quantifier and location.
-- Returns Left at ambiguity error, and Right otherwise.
--searchObjects :: State -> Object -> Quantifier ->
--                 Maybe Location -> Either [[(Id, Object)]] [(Id, Object)]
searchObjects :: Object -> Quantifier -> Maybe Location -> Interpretation [(Id, Object)]
searchObjects obj quant mloc = do
  state <- ask
  case mloc of
    Nothing ->
      case quant of
        All -> return $ foundObjects state
        Any -> return $ foundObjects state
        The -> return $ foundObjects state
       -- The -> case length (foundObjects state) of
       --   1 -> return $ take 1 (foundObjects state)
       --   _ -> throwError $ AmbiguityError $ map fst (foundObjects state) -- <------ ERROR
    Just loc ->
      case quant of
        All -> return $ foundObjects state
        Any -> do
          filterM (\(i,o) -> locationHolds state (i,o) loc) (foundObjects state)
        The -> do
          filterM (\(i,o) -> locationHolds state (i,o) loc) (foundObjects state)
          -- foundObjects' <- filterM (\(i,o) -> locationHolds state (i,o) loc) (foundObjects state)
          -- case length foundObjects' of
          --   1 -> return $ take 1 foundObjects'
          --   0 -> return []
          --   _ -> error "Ambiguity" --Left $ map (: []) foundObjects'
  where 
    ids state = case holding state of
            Nothing      -> concat $ world state
            Just holdId  -> holdId : concat (world state)
    exists (i,_) state    = i `elem` (ids state)
    isMatching (_, o) = obj == o
    foundObjects state = filter (\i -> exists i state) . filter isMatching $ M.assocs (objects state)

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
    objPos'           = findObjPos ide (world state)
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
    Floor -> return []
    BasicEntity q obj -> do
      found <- searchObjects obj q Nothing
      return $ map fst found
    RelativeEntity q obj loc -> do
      found <- searchObjects obj q (Just loc)
      return $ map fst found

-- | Returns the column index of the object id, if any.
column :: State -> Id -> Maybe [Id]
column state ident = fmap (\pos -> world state !! fst pos) i
  where i = findObjPos ident (world state)
