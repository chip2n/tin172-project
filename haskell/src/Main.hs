#!/usr/bin/env runhaskell

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where

import Shrdlite.Grammar
import Shrdlite.CombinatorParser
import Text.JSON
import Data.Maybe

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
    goals     = case interpretAll state trees of
                  Left r   -> error $ "Interpretation error: " ++ show r
                  Right gs -> resolveAmbiguity state gs
    goals'    = case goals of -- validate goals
                  Left err -> error $ show err -- TODO: Send control question
                  Right g  -> [g]
    plan = if length goals' > 0
             then Planner.solve world holding objects goals' :: Maybe Plan
             else Nothing

    output
      | null trees          = "Parse error!"
      | length goals' >= 2  = "Ambiguity error!" -- just OR them
      | null goals'         = "Interpretation error!"
      | isNothing plan      = "Planning error!"
      | fromJust plan == [] = "Way ahead of you!"
      | otherwise           = "Much wow!"

    result = [ ("utterance",    showJSON utterance)
             , ("trees",        showJSON (map show trees))
             , ("goals",        showJSON $ map show goals')
             , ("plan",         showJSON $ fromMaybe [] plan)
             , ("output",       showJSON output)
             , ("receivedJSON", showJSON $ jsinput)
             ]
