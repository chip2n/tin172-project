module Shrdlite.Grammar (
  -- * Types
    Command    (..)
  , Location   (..)
  , Entity     (..)
  , Object     (..)
  , Quantifier (..)
  , Relation   (..)
  , Size       (..)
  , Color      (..)
  , Form       (..)
  , SParser

  -- * Functions
  , size
  , color
  , command
  ) where

import Shrdlite.CombinatorParser
import Text.JSON()

type SParser = Parser String

-- Data types

data Command = Take Entity | Put Location | Move Entity Location
               deriving (Eq, Ord, Show)

data Location = Relative Relation Entity
                deriving (Eq, Ord, Show)

data Entity = Floor | BasicEntity Quantifier Object | RelativeEntity Quantifier Object Location
              deriving (Eq, Ord, Show)

data Object = Object Size Color Form
              deriving (Eq, Ord, Show)

data Quantifier = The | Any | All
                  deriving (Eq, Ord, Show)

data Relation = Beside | Leftof | Rightof | Above | Ontop | Under | Inside
                deriving (Eq, Ord, Show)

data Size = AnySize | Small | Large
            deriving (Ord, Show)

instance Eq Size where
    AnySize == _       = True
    _       == AnySize = True
    Small   == Small   = True
    Large   == Large   = True
    _       == _       = False

data Color = AnyColor | Black | White | Blue | Green | Yellow | Red
             deriving (Ord, Show)

instance Eq Color where
    AnyColor == _        = True
    _        == AnyColor = True
    Black    == Black    = True
    White    == White    = True
    Blue     == Blue     = True
    Green    == Green    = True
    Yellow   == Yellow   = True
    Red      == Red      = True
    _        == _        = False

data Form = AnyForm | Brick | Plank | Ball | Pyramid | Box | Table
            deriving (Ord, Show)

instance Eq Form where
    AnyForm == _       = True
    _       == AnyForm = True
    Brick   == Brick   = True
    Plank   == Plank   = True
    Ball    == Ball    = True
    Pyramid == Pyramid = True
    Box     == Box     = True
    Table   == Table   = True
    _       == _       = False

-- Grammar rules

command :: SParser Command
command = mkCommand $
          Take <$> (takeVerb *> entity)
          <|>
          Put  <$> (moveVerb *> itPron *> location)
          <|>
          Move <$> (moveVerb *> entity) <*> location

location :: SParser Location
location = Relative <$> relation <*> entity

entity :: SParser Entity
entity = Floor <$ theFloor
         <|>
         numberAgreement (liftA2 BasicEntity <$> quantifier <*> object)
         <|>
         numberAgreement (liftA3 RelativeEntity <$> quantifier <*> object <*> relative_clause)
    where
      relative_clause n = thatIs n *> location

object :: Number -> SParser Object
object n = Object <$> (size <|> pure AnySize) <*> (color <|> pure AnyColor) <*> form n
           <|>
           flip Object <$> color <*> size <*> form n

-- Lexical rules

quantifier :: Number -> SParser Quantifier
quantifier Sg = lexicon [(The, ["the"]),
                         (Any, ["a", "an", "any"]),
                         (All, ["every"])]
quantifier Pl = lexicon [(All, ["all"])]

relation :: SParser Relation
relation = lexicon [(Beside,  ["beside"]),
                  (Leftof,  ["left of", "to the left of"]),
                  (Rightof, ["right of", "to the right of"]),
                  (Above,   ["above"]),
                  (Ontop,   ["on top of", "on"]),
                  (Under,   ["under"]),
                  (Inside,  ["inside", "in", "into"])]

-- | Parses the size from a string
size :: SParser Size
size = lexicon [(Small,  ["small", "tiny"]),
                (Large,  ["large", "big"])]

-- | Parses the colour from a string
color :: SParser Color
color = lexicon [(Black,  ["black"]),
                 (White,  ["white"]),
                 (Blue,   ["blue"]),
                 (Green,  ["green"]),
                 (Yellow, ["yellow"]),
                 (Red,    ["red"])]

-- | Parses the form from a string
form :: Number -> SParser Form
form n = lexicon [(AnyForm, [regNoun n "object", regNoun n "thing", regNoun n "form"]),
                  (Brick,   [regNoun n "brick"]),
                  (Plank,   [regNoun n "plank"]),
                  (Ball,    [regNoun n "ball"]),
                  (Pyramid, [regNoun n "pyramid"]),
                  (Box,     [mkNoun  n "box" "boxes"]),
                  (Table,   [regNoun n "table"])]

-- Lexicon

data Number = Sg | Pl
              deriving (Eq, Ord, Show)

numberAgreement :: (Number -> SParser a) -> SParser a
numberAgreement p = p Sg <|> p Pl

regNoun :: Number -> String -> String
regNoun n s = mkNoun n s (s ++ "s")

mkNoun :: Number -> String -> String -> String
mkNoun Sg sg _ = sg
mkNoun Pl _ pl = pl

mkCommand :: SParser Command -> SParser Command
mkCommand prs = lexicon [((), ["", "will you", "can you", "could you"])] *>
                lexicon [((), ["", "please"])] *>
                prs <*
                lexicon [((), ["", "please"])]

theFloor :: SParser ()
theFloor = lexicon [((), ["the floor"])]

thatIs :: Number -> SParser ()
thatIs Sg = lexicon [((), ["", "that is"])]
thatIs Pl = lexicon [((), ["", "that are"])]

moveVerb :: SParser ()
moveVerb = lexicon [((), ["move", "put", "drop"])]

takeVerb :: SParser ()
takeVerb = lexicon [((), ["take", "grasp", "pick up"])]

itPron :: SParser ()
itPron = lexicon [((), ["it"])]
