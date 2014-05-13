module Shrdlite.Common where

import Shrdlite.Grammar
import Data.List (elemIndex)
import qualified Data.Map as M
import Text.JSON
import Shrdlite.CombinatorParser

type Utterance = [String]
type Id = String
type World = [[Id]]
--type Objects = JSObject JSValue
type Objects = M.Map Id Object
data Goal = TakeGoal GoalObject
          | PutGoal Relation GoalObject GoalObject
          deriving (Show, Eq, Ord)
data GoalObject = Flr | Obj Id deriving (Show, Eq, Ord)
type Plan = [String]
data State = State { world :: World, holding :: Maybe Id, objects :: Objects }
  deriving (Show)
type WorldHolding = (World, Maybe Id)
type WorldChange = (Int, [Id]) -- The changed column id and the new column

instance Eq State where
  s1 == s2 = world s1 == world s2 && holding s1 == holding s2
  s1 /= s2 = world s1 /= world s2 || holding s1 /= holding s2

instance Ord State where
  compare s1 s2 = compare (world s1) (world s2)

-- |Finds the column and height of the object with the provided id
findObjPos :: Id -> World -> Maybe (Int,Int)
findObjPos = findObjPos' 0

findObjPos' :: Int -> Id -> World -> Maybe (Int,Int)
findObjPos' _ _ [] = Nothing
findObjPos' x i (ids:idss) = case elemIndex i ids of
  Nothing -> findObjPos' (x+1) i idss
  Just y  -> Just (x,y)

parseObjects :: JSObject JSValue -> Objects
parseObjects obj = M.fromList $ map parseValue $ fromJSObject obj

parseValue :: (Id, JSValue) -> (Id, Object)
parseValue (s, JSObject val) = (s, obj)
 where
    objForm = head $ parse form' [ok (valFromObj "form" val) :: String]
    objSize = head $ parse size [ok (valFromObj "size" val) :: String]
    objColor = head $ parse color [ok (valFromObj "color" val) :: String]
    obj = Object objSize objColor objForm
parseValue (_, _) = error "Wow, did you screw up"

form' :: SParser Form
form' = lexicon [(Brick,   ["brick"]),
                 (Plank,   ["plank"]),
                 (Ball,    ["ball"]),
                 (Pyramid, ["pyramid"]),
                 (Box,     ["box"]),
                 (Table,   ["table"])]

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

maybeOk :: Result a -> Maybe a
maybeOk (Ok res) = Just res
maybeOk (Error _) = Nothing
