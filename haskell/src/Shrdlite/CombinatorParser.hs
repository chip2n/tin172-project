module Shrdlite.CombinatorParser (
  module Shrdlite.CombinatorParser,
  module Control.Applicative
) where

import Control.Applicative
import qualified Data.Foldable as F

newtype Parser t a = Parser ([t] -> [([t], a)])

parse :: Parser t a -> [t] -> [a]
parse (Parser p) ts = [a | ([], a) <- p ts]

instance Functor (Parser t) where
    fmap f (Parser p) = Parser (\ts -> [(ts', f a) | (ts', a) <- p ts])

instance Applicative (Parser t) where
    pure a = Parser (\ts -> [(ts, a)])
    Parser p <*> Parser q = Parser (\ts -> [(ts'', f a) | (ts', f) <- p ts, (ts'', a) <- q ts'])

instance Alternative (Parser t) where
    empty = Parser (const [])
    Parser p <|> Parser q = Parser (\ts -> p ts ++ q ts)

token :: Eq t => t -> Parser t t
token t = Parser (\ts -> case ts of
                           t':ts' | t == t' -> [(ts', t')]
                           _ -> [])

-- Convenience parsers

anyof :: [Parser t a] -> Parser t a
anyof = F.asum

tokens :: Eq t => [t] -> Parser t [t]
tokens = foldr (\ x -> (<*>) ((:) <$> token x)) (pure [])
--tokens [] = pure []
--tokens (x:xs) = (:) <$> token x <*> tokens xs

lexicon :: [(a, [String])] -> Parser String a
lexicon alts = anyof [a <$ anyof (map (tokens.words) alt) | (a, alt) <- alts]
