#!/usr/bin/env runhaskell

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
import qualified Data.Set as S
import Data.Graph.AStar

-- Our own modules
import Shrdlite.Planner as Planner
import Shrdlite.Common as Common
import Shrdlite.Interpreter as Interpreter

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
      plan      = Planner.solve world holding objects (head goals) :: Plan

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


ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

maybeOk :: Result a -> Maybe a
maybeOk (Ok res) = Just res
maybeOk (Error _) = Nothing
