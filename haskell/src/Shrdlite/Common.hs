module Shrdlite.Common (
   -- * Types
     Utterance
   , Id
   , Objects
   , Plan
   , World
   , WorldChange
   , WorldHolding

   -- * Datatypes
   , Goal (..)
   , GoalObject (..)
   , ValidatedGoal (..)
   , State (..)
   , ShrdliteError (..)

   -- * Functions
   , shrdliteErrorMsg
   , isAmbiguityError
   , findObjPos
   , maybeOk
   , ok
   , parseObjects
   , validate
   , validateAllLaws
   , getSize
   , getColor
   , getForm
) where

import Control.Monad.Error
import Data.List (elemIndex)
import Text.JSON
import qualified Data.Map as M

import Shrdlite.CombinatorParser
import Shrdlite.Grammar

type Id           = String
type Objects      = M.Map Id Object
type Plan         = [String]
type Utterance    = [String]
type World        = [[Id]]
type WorldChange  = (Int, [Id]) -- The changed column id and the new column
type WorldHolding = (World, Maybe Id)

data Goal = TakeGoal GoalObject
          | MoveGoal Relation GoalObject GoalObject
          deriving (Show, Eq, Ord)
data GoalObject = Flr | Obj Quantifier Id deriving (Show, Eq, Ord)
data ValidatedGoal = SingleGoal Goal -- This goal has to be fulfilled
                   | OrGoal [Goal] -- Any of these goals are acceptable
                   | AndGoal [Goal] -- All these goals have to be fulfilled
data State = State { world :: World, holding :: Maybe Id, objects :: Objects }
  deriving (Show)

instance Eq State where
  s1 == s2 = world s1 == world s2 && holding s1 == holding s2
  s1 /= s2 = world s1 /= world s2 || holding s1 /= holding s2

instance Ord State where
  compare s1 s2 = compare (world s1) (world s2)

-- | Data structure for different errors that can show up during a Shrdlite run
data ShrdliteError = EntityError String
                   | AmbiguityError [Goal] String
                   | OtherError String
                   deriving (Show, Eq, Ord)

-- | Gets the message from en error
shrdliteErrorMsg :: ShrdliteError -> String
shrdliteErrorMsg (EntityError msg)      = "Interpretation error: " ++ msg
shrdliteErrorMsg (AmbiguityError _ msg) = "Ambiguity error: " ++ msg
shrdliteErrorMsg (OtherError  msg)      = "Error: " ++ msg

-- | Checks wether the provided error is an AmbiguityError.
isAmbiguityError :: ShrdliteError -> Bool
isAmbiguityError (AmbiguityError _ _) = True
isAmbiguityError _                  = False

-- | For the ErrorT monad.
instance Error ShrdliteError where
    noMsg  = OtherError ""
    strMsg = OtherError

getSize :: Object -> Size
getSize (Object s _ _) = s

getColor :: Object -> Color
getColor (Object _ c _) = c

getForm :: Object -> Form
getForm (Object _ _ f) = f

-- | Finds the column and height of the object with the provided id
findObjPos :: Id -> World -> Maybe (Int,Int)
findObjPos = findObjPos' 0

-- Helper for findObjPos
findObjPos' :: Int -> Id -> World -> Maybe (Int,Int)
findObjPos' _ _ [] = Nothing
findObjPos' x i (ids:idss) = case elemIndex i ids of
  Nothing -> findObjPos' (x+1) i idss
  Just y  -> Just (x,y)

-- | Extracts an Object from a JSObject
parseObjects :: JSObject JSValue -> Objects
parseObjects obj = M.fromList $ map parseValue $ fromJSObject obj

-- Helper for parseObjects
-- Extracts an Object from a JSValue with a given id
parseValue :: (Id, JSValue) -> (Id, Object)
parseValue (s, JSObject val) = (s, obj)
 where
    objForm = head $ parse form' [ok (valFromObj "form" val) :: String]
    objSize = head $ parse size [ok (valFromObj "size" val) :: String]
    objColor = head $ parse color [ok (valFromObj "color" val) :: String]
    obj = Object objSize objColor objForm
parseValue (_, _) = error "Wow, did you screw up"

-- Helper for parseValue
form' :: SParser Form
form' = lexicon [(Brick,   ["brick"]),
                 (Plank,   ["plank"]),
                 (Ball,    ["ball"]),
                 (Pyramid, ["pyramid"]),
                 (Box,     ["box"]),
                 (Table,   ["table"])]

-- | Unwrapper for the result type
ok :: Result a -> a
ok (Ok res)    = res
ok (Error err) = error err

-- | Converts a Result into a Maybe
-- Returns Nothing in case of Error otherwise it returns Just value
maybeOk :: Result a -> Maybe a
maybeOk (Ok res)  = Just res
maybeOk (Error _) = Nothing

-- | Tests whether the first object can be placed immediately above the second
-- object without violating the given constraints.
validate :: Object -> Object -> Bool
validate _                        (Object _     _ Ball)  = False
validate (Object s1    _ Ball)    (Object s2    _ Box)   = s1 <= s2
validate (Object _     _ Ball)    _                      = False
validate (Object s1    _ Pyramid) (Object s2    _ Box)   = s1 < s2
validate (Object s1    _ Plank)   (Object s2    _ Box)   = s1 < s2
validate (Object s1    _ Box)     (Object s2    _ Plank) = s1 == s2
validate (Object s1    _ Box)     (Object s2    _ Table) = s1 == s2
validate (Object Large _ Box)     (Object Large _ Brick) = True
validate (Object s1    _ Box)     (Object s2    _ Box  ) = s1 < s2
validate (Object _     _ Box)     _                      = False
validate (Object s1    _ _)       (Object s2    _ _)     = s1 <= s2

-- | Validates all physical laws. This does not look if the objects exists in the
-- world, and it does not care about their locations.
validateAllLaws :: Relation -> Object -> Object -> Bool
validateAllLaws relation o1 o2 =
  case relation of
    Beside  -> True
    Leftof  -> True
    Rightof -> True
    Above   -> getForm o2 /= Ball && getSize o1 <= getSize o2
    Ontop   -> getForm o2 /= Box && validate o1 o2
    Under   -> getForm o1 /= Ball && getSize o1 >= getSize o2
    Inside  -> getForm o2 == Box && validate o1 o2
