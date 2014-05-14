#!/usr/bin/env runhaskell

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where

import Shrdlite.Grammar
import Shrdlite.CombinatorParser
import Text.JSON

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


showGoals :: [Goal] -> [String]
showGoals goals = map show goals


