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

-- | Reads from standard input a JSON string, decodes it and parses it into
-- corresponding data structure. After this is done, it runs the interpreter,
-- ambiguity resolver and planner on these structures, and finally writes the
-- JSON output to standard output.
main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode

jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
  where
    utterance = ok      (valFromObj "utterance" jsinput)         :: Utterance
    wrld      = ok      (valFromObj "world"     jsinput)         :: World
    hldng     = maybeOk (valFromObj "holding"   jsinput)         :: Maybe Id
    obj       = parseObjects $ ok (valFromObj "objects" jsinput) :: Objects
    state     = State wrld hldng obj    
    trees     = parse command utterance :: [Command]
    goals     = case interpretAll state trees of
                  Left r   -> error $ "Interpretation error: " ++ show r
                  Right gs -> resolveAmbiguity state gs
    goals'    = case goals of -- validate goals
                  Left err -> error $ show err -- TODO: Send control question
                  Right g  -> [g]
    plan = if not (null goals')
             then Planner.solve wrld hldng obj goals' :: Maybe Plan
             else Nothing

    output
      | null trees           = "Parse error!"
      | length goals' >= 2   = "Ambiguity error!" -- just OR them
      | null goals'          = "Interpretation error!"
      | isNothing plan       = "Planning error!"
      | null (fromJust plan) = "Way ahead of you!"
      | otherwise            = "Much wow!"

    result = [ ("utterance",    showJSON utterance)
             , ("trees",        showJSON (map show trees))
             , ("goals",        showJSON $ map show goals')
             , ("plan",         showJSON $ fromMaybe [] plan)
             , ("output",       showJSON output)
             , ("receivedJSON", showJSON jsinput)
             ]
