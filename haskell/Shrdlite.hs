#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where 

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (elemIndex)
import Data.Maybe
import Control.Monad
import qualified Data.Map as M

type Utterance = [String]
type Id = String
type World = [[Id]]
--type Objects = JSObject JSValue
type Objects = M.Map Id Object
data Goal = TakeGoal GoalObject
          | PutGoal Relation GoalObject GoalObject
          deriving (Show)
data GoalObject = Flr Int | Obj Id deriving (Show)
type Plan = [String]
data State = State { world :: World, holding :: Maybe Id, objects :: Objects }


main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode


jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where 
      utterance = ok (valFromObj "utterance" jsinput) :: Utterance
      world     = ok (valFromObj "world"     jsinput) :: World
      holding   = maybeOk (valFromObj "holding"   jsinput) :: Maybe Id
      objects   = parseObjects $ ok (valFromObj "objects"   jsinput) :: Objects
      state     = State world holding objects

      trees     = parse command utterance :: [Command]
      goals     = concat . map (interpret state) $ trees
      plan      = solve world holding objects (head goals) :: Plan

      output
        | null trees        = "Parse error!"
        | null goals        = "Interpretation error!"
        | length goals >= 2 = "Ambiguity error!"
        | null plan         = "Planning error!"
        | otherwise         = "Much wow!"

      result = [("utterance",
                   showJSON utterance),
                ("trees",     showJSON (map show trees)),
                ("goals",     if not (null trees)
                                then showJSON (showGoals goals)
                                else JSNull),
                ("plan",      if length goals == 1
                                then showJSON plan
                                else JSNull),
                ("output",    showJSON output),
                ("receivedJSON", showJSON $ jsinput)
               ]


parseObjects :: JSObject JSValue -> Objects
parseObjects obj = M.fromList $ map parseValue $ fromJSObject obj

parseValue :: (Id, JSValue) -> (Id, Object)
parseValue (s, JSObject val) = (s, obj)
 where
    objForm = head $ parse form' $ [ok (valFromObj "form" val) :: String]
    objSize = head $ parse size $ [ok (valFromObj "size" val) :: String]
    objColor = head $ parse color $ [ok (valFromObj "color" val) :: String]
    obj = Object objSize objColor objForm

form' :: SParser Form
form' = lexicon [(Brick,   ["brick"]),
                 (Plank,   ["plank"]),
                 (Ball,    ["ball"]),
                 (Pyramid, ["pyramid"]),
                 (Box,     ["box"]),
                 (Table,   ["table"])]

showGoals :: [Goal] -> [String]
showGoals goals = map show goals

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
searchObjects :: State ->
                 Object -> Quantifier -> Maybe Location -> Either [[(Id, Object)]] [(Id, Object)]
searchObjects state obj quantifier loc =
    case loc of
        Nothing ->
            case quantifier of
                All -> Right foundObjects
                Any -> Right $ take 1 foundObjects
                The -> case length foundObjects of
                           1 -> Right $ take 1 foundObjects
                           _ -> Left $ map (\a -> [a]) foundObjects
        Just location ->
            case quantifier of
                All -> Right foundObjects
                Any -> Right $ take 1 foundObjects
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
        leftof i1 i2     = fromMaybe False $ liftM2 (\a b -> a - b == -1) (findObjColumn i1) (findObjColumn i2)
        form' (Object _ _ f) = f

        
findEntity :: State -> Entity -> [Id]
findEntity state entity =
    case entity of
        Floor -> []
        BasicEntity q obj -> 
            case searchObjects state obj q Nothing of
                Left _ -> error "Ambiguity error."
                Right objs -> map fst objs
        RelativeEntity q obj loc -> undefined -- searchObjects world Nothing objects obj q loc

-- | Returns the column index of the object id, if any.
column :: State -> Id -> Maybe [Id]
column state id = fmap (\pos -> world state !! fst pos) i
  where i = findObjPos id (world state)

findObjPos :: Id -> World -> Maybe (Int,Int)
findObjPos = findObjPos' 0

findObjPos' :: Int -> Id -> World -> Maybe (Int,Int)
findObjPos' _ _ [] = Nothing
findObjPos' x i (ids:idss) = case elemIndex i ids of
  Nothing -> findObjPos' (x+1) i idss
  Just y  -> Just (x,y)

-- | Creates a list of moves which together creates a "Plan". The plan can
-- consist of messages to the user and commands in the form of
-- "pick FLOOR_SPACE" and "drop FLOOR_SPACE"
solve :: World -> Maybe Id -> Objects -> Goal -> Plan
solve world holding objects goal =
    [ "I totally picked it up . . ."
    , "pick " ++ show col
    , ". . . and I dropped it down."
    , "drop " ++ show col ]
    where
      Just col = case goal of
        (TakeGoal (Obj i)) -> fmap fst $ findObjPos i world
      -- Just col = findIndex (not . null) world


-- | Checks if the goal is fulfilled in the world state
check :: World -> Goal -> Bool
check = undefined

-- | Checks which plans is the best one, according to some heuristic
bestPlan :: [Plan] -> Plan
bestPlan = undefined

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

maybeOk :: Result a -> Maybe a
maybeOk (Ok res) = Just res
maybeOk (Error _) = Nothing
