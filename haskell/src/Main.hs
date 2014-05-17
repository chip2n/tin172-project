#!/usr/bin/env runhaskell

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where

import Shrdlite.Grammar
import Shrdlite.CombinatorParser
import Text.JSON
import Debug.Trace
import qualified Data.Map as M

-- Our own modules
import Shrdlite.Common as Common
import Shrdlite.Interpreter as Interpreter
import Shrdlite.AmbiguityResolver as Resolver
import Shrdlite.Planner as Planner

main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode
                                   
jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
  where 
    utterance = ok      (valFromObj "utterance" jsinput)         :: Utterance
    world     = ok      (valFromObj "world"     jsinput)         :: World
    holding   = maybeOk (valFromObj "holding"   jsinput)         :: Maybe Id
    objects   = parseObjects $ ok (valFromObj "objects" jsinput) :: Objects
    state     = State world holding objects
    trees     = parse command utterance :: [Command]
    --goals     = concat . map (interpret state) $ trees
    goals     = case interpretAll state trees of
                  Left r   -> error $ "Interpretation error: " ++ show r
                  Right gs -> gs
    --goals'    = case map (resolveAmbiguity state) goals of
    --              Left err -> error $ err -- TODO: Send control question
    --              Right g  -> [g]
    goals'   = case goals of
                  [] -> []
                  [g] -> g
                  _   -> []
    --if length goals > 1 then error "Ambiguity not handled yet" else head goals
    plan = if length goals' == 1
             then Planner.solve world holding objects goals' :: Plan
             else []

    output
      | null trees        = "Parse error!"
      | length goals' >= 2 = "Ambiguity error!"
      | null goals'       = "Interpretation error!"
      | null plan         = "Planning error!"
      | otherwise         = "Much wow!"

    result = [ ("utterance",    showJSON utterance)
             , ("trees",        showJSON (map show trees))
             , ("goals",        showJSON $ map show goals)
             , ("plan",         showJSON plan)
             , ("output",       showJSON output)
             , ("receivedJSON", showJSON $ jsinput)
             ]
