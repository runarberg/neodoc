-- |
-- | This module defines commonly used Parser-combinators
-- |

module Neodoc.Spec.Parser.Base where

import Prelude
import Control.Alt ((<|>))
import Data.List (List(), many)
import Text.Parsing.Parser (PState(..), ParserT(..), Result(..)) as P
import Text.Parsing.Parser.Pos (Position(..)) as P
import Text.Parsing.Parser.String (satisfy, char, string) as P
import Data.Array as A
import Data.Char (toLower, toUpper, toCharCode)
import Data.String (toCharArray, fromCharArray)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Either (Either(Right))
import Debug.Trace (traceShow)
import Control.MonadPlus (guard)

debug :: ∀ a m. (Show a, Monad m) => a -> m Unit
debug x = traceShow x $ const $ pure unit

-- | Return the current parser position
getPosition :: ∀ a m. (Monad m) => P.ParserT a m P.Position
getPosition = P.ParserT $ \(P.PState s pos) ->
  pure (P.Result s (Right pos) false pos)

sof :: ∀ a m. (Monad m) => P.ParserT a m Unit
sof = do
  P.Position line col <- getPosition
  guard $ col == 1 && line == 1

-- | Return the current parser column
getCol :: ∀ a m. (Monad m) => P.ParserT a m Int
getCol = do
  P.Position _ col <- getPosition
  pure col

-- | Return the current parser row
-- | XXX: Use either `line` or `row` - not both!
getRow :: ∀ a m. (Monad m) => P.ParserT a m Int
getRow = do
  P.Position row _ <- getPosition
  pure row

-- | Return the current parser row
-- | XXX: Use either `line` or `row` - not both!
getLine :: ∀ a m. (Monad m) => P.ParserT a m Int
getLine = getRow

tryMaybe :: ∀ s m a. (Monad m) => P.ParserT s m a -> P.ParserT s m (Maybe a)
tryMaybe p = (Just <$> p) <|> (pure Nothing)

-- | Return the current parser position
getInput :: ∀ a m. (Monad m, Show a) => P.ParserT a m a
getInput = P.ParserT $ \(P.PState s pos) ->
  pure (P.Result s (Right s) false pos)

traceInput :: ∀ a m. (Show a, Monad m) => P.ParserT a m Unit
traceInput = getInput >>= debug

satisfyCode :: ∀ m. (Monad m) => (Int -> Boolean) -> P.ParserT String m Char
satisfyCode f = P.satisfy \c -> f (toCharCode c)

isDigit :: Int -> Boolean
isDigit c = c > 47 && c < 58

isLowerAlpha :: Int -> Boolean
isLowerAlpha c = c > 96 && c < 123

isUpperAlpha :: Int -> Boolean
isUpperAlpha c = c > 64 && c < 91

lower :: ∀ m. (Monad m) => P.ParserT String m Char
lower = P.satisfy \c -> c == toLower c

upper :: ∀ m. (Monad m) => P.ParserT String m Char
upper = P.satisfy \c -> c == toUpper c

digit :: ∀ m. (Monad m) => P.ParserT String m Char
digit = satisfyCode \c -> c > 47 && c < 58

alpha :: ∀ m. (Monad m) => P.ParserT String m Char
alpha = satisfyCode \c -> (c > 64 && c < 91) || (c > 96 && c < 123)

upperAlpha :: ∀ m. (Monad m) => P.ParserT String m Char
upperAlpha = satisfyCode isUpperAlpha

lowerAlpha :: ∀ m. (Monad m) => P.ParserT String m Char
lowerAlpha = satisfyCode isLowerAlpha

lowerAlphaNum :: ∀ m. (Monad m) => P.ParserT String m Char
lowerAlphaNum = satisfyCode \c -> isLowerAlpha c || isDigit c

upperAlphaNum :: ∀ m. (Monad m) => P.ParserT String m Char
upperAlphaNum = satisfyCode \c -> isUpperAlpha c || isDigit c

alphaNum :: ∀ m. (Monad m) => P.ParserT String m Char
alphaNum = satisfyCode \c -> isUpperAlpha c || isLowerAlpha c || isDigit c

space :: ∀ m. (Monad m) => P.ParserT String m Char
space = P.satisfy \c -> c == ' ' || c == '\t'

spaces :: ∀ m. (Monad m) => P.ParserT String m (List Char)
spaces = many space

eol :: ∀ m. (Monad m) => P.ParserT String m Unit
eol = (void $ P.string "\r\n") <|> (void $ P.char '\n')

string' :: ∀ m. (Monad m) => String -> P.ParserT String m String
string' s = fromCharArray <$> A.foldM step [] (toCharArray s)
  where
    step acc x = do
      (acc <> _) <<< A.singleton <$> P.satisfy \c -> toLower c == toLower x
