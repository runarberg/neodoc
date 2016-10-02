module Neodoc.Spec.TokenParser where

import Prelude
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Data.List as L
import Data.Monoid (mempty)
import Data.Pretty
import Data.Functor (($>))
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Monad.Transformerless.State (State, evalState)
import Control.MonadPlus (guard)
import Data.Either (Either(..), fromRight)
import Data.Identity (Identity())
import Data.Foldable (foldMap)
import Data.List (List(..), many, catMaybes, toUnfoldable, (:), some)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (fromCharArray, trim)
import Data.String (singleton, toUpper, split, joinWith) as String
import Data.String.Regex (Regex(), regex)
import Data.String.Regex (test, parseFlags, replace) as Regex
import Partial.Unsafe (unsafePartial)
import Data.String.Ext ((^=), (~~))
import Text.Parsing.Parser (
  ParseError(..), Parser, PState(..), ParserT(..), Result(..)
, runParserT, parseFailed, fail, runParser, unParserT) as P
import Text.Parsing.Parser.Combinators ((<??>))
import Text.Parsing.Parser.Combinators (
  (<?>), notFollowedBy, try, choice, lookAhead, optional, between, manyTill
, option) as P
import Text.Parsing.Parser.Pos (Position(..), initialPos) as P
import Text.Parsing.Parser.String (
  skipSpaces, anyChar, string, char, oneOf, whiteSpace, eof, noneOf
, satisfy) as P

import Neodoc.Spec.Error (SpecParseError(..))
import Neodoc.Spec.Token
import Neodoc.Spec.Parser.Base (
  lowerAlphaNum, alphaNum, alpha, space, lowerAlpha, upperAlpha, string'
, getPosition, getInput, spaces, eol)
import Neodoc.Spec.ParserState (ParserState(..))
import Neodoc.Spec.ParserState as ParserState
import Neodoc.Spec.Lexer2

-- | Parser that  parses a stream of tokens
type TokenParser a = P.ParserT (List PositionedToken) (State ParserState) a

-- | Test the token at the head of the stream
token :: ∀ a. (Token -> Maybe a) -> TokenParser a
token test = P.ParserT $ \(P.PState toks pos) ->
  pure $ case toks of
    x@(PositionedToken ppos tok):xs ->
      case test tok of
        Just a ->
          let nextpos =
                case xs of
                  (PositionedToken npos _):_ -> P.Position 1 npos
                  Nil -> P.Position 1 ppos
          in P.Result xs (Right a) true pos
        -- XXX: Fix this error message, it makes no sense!
        Nothing -> P.parseFailed toks pos "a better error message!"
    _ -> P.parseFailed toks pos "expected token, met EOF"

-- | Match the token at the head of the stream
match :: Token -> TokenParser Unit
match tok = token (guard <<< (_ == tok)) P.<?> pretty tok

anyToken :: TokenParser Token
anyToken = token $ Just

eof :: TokenParser Unit
eof = "EOF" <??> P.notFollowedBy anyToken

lparen :: TokenParser Unit
lparen = match LParen

rparen :: TokenParser Unit
rparen = match RParen

lsquare :: TokenParser Unit
lsquare = match LSquare

rsquare :: TokenParser Unit
rsquare = match RSquare

dash :: TokenParser Unit
dash = match Dash

doubleDash :: TokenParser Unit
doubleDash = match DoubleDash

vbar :: TokenParser Unit
vbar = match VBar

comma :: TokenParser Unit
comma = match Comma

colon :: TokenParser Unit
colon = match Colon

newline :: TokenParser Unit
newline = match Newline

tripleDot :: TokenParser Unit
tripleDot = match TripleDot

garbage :: TokenParser Unit
garbage = "garbage" <??> token go
  where
    go (Garbage _) = Just unit
    go _           = Nothing

lopt :: TokenParser { name :: String
                    , arg  :: Maybe OptionArgumentObj
                    }
lopt = "long-option" <??> token go
  where
    go (LOpt n a) = Just { name: n, arg: a }
    go _          = Nothing

sopt :: TokenParser { chars :: NonEmpty Array Char
                    , arg   :: Maybe OptionArgumentObj
                    }
sopt = "short-option" <??> token go
  where
    go (SOpt cs a) = Just { chars: cs , arg: a }
    go _ = Nothing

name :: TokenParser String
name = "name" <??> token go
  where
    go (Name n) = Just n
    go _        = Nothing

tag :: String -> TokenParser String
tag s = ("tag: " ~~ s) <??> token go
  where
    go (Tag k v) | k ^= s = Just v
    go _                  = Nothing

reference :: TokenParser String
reference = "reference" <??> token go
  where
    go (Reference r) = Just r
    go _             = Nothing

angleName :: TokenParser String
angleName = "<name>" <??> token go
  where
    go (AngleName n) = Just n
    go _             = Nothing

shoutName :: TokenParser String
shoutName = "NAME" <??> token go
  where
    go (ShoutName n) = Just n
    go _             = Nothing

-- | Return the next token's position w/o consuming anything
nextTokPos :: TokenParser P.Position
nextTokPos = P.ParserT $ \(P.PState toks pos) ->
  pure $ case toks of
    x@(PositionedToken ppos _):xs ->
      P.Result toks (Right (P.Position 1 ppos)) false pos
    otherwise -> P.parseFailed toks pos "expected token, met EOF"

runTokenParser
  :: ∀ a
   . List PositionedToken
  -> TokenParser a
  -> Either P.ParseError a
runTokenParser s =
  flip evalState ParserState.initialState
    <<< P.runParserT (P.PState s P.initialPos)

