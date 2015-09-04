module Docopt.Parser.Lexer where

import Prelude
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Token as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import qualified Data.Array as A
import Data.Char (toString, toLower, toUpper)
import Data.String (fromCharArray)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Docopt.Parser.Base

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | LAngle
  | RAngle
  | Dot
  | Dash
  | Name String

prettyPrintToken :: Token -> String
prettyPrintToken LParen  = "("
prettyPrintToken RParen  = ")"
prettyPrintToken LSquare = "["
prettyPrintToken RSquare = "]"
prettyPrintToken LAngle  = "<"
prettyPrintToken RAngle  = ">"
prettyPrintToken Dash    = "-"
prettyPrintToken Dot     = "."
prettyPrintToken (Name name) = name

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken { sourcePos=pos, token=tok }) =
    (show tok) ++ " at " ++ (show pos)

parseTokens :: P.Parser String (L.List PositionedToken)
parseTokens = do
  P.skipSpaces
  L.many parsePositionedToken
  <* P.eof

parsePositionedToken :: P.Parser String PositionedToken
parsePositionedToken = P.try $ do
  pos <- getPosition
  tok <- parseToken
  return $ PositionedToken { sourcePos: pos, token: tok }

parseToken :: P.Parser String Token
parseToken = P.choice
  [ P.try $ P.char '(' *> pure LParen
  , P.try $ P.char ')' *> pure RParen
  , P.try $ P.char '[' *> pure LSquare
  , P.try $ P.char ']' *> pure RSquare
  , P.try $ P.char '<' *> pure LAngle
  , P.try $ P.char '>' *> pure RAngle
  , P.try $ P.char '-' *> pure Dash
  , P.try $ P.char '.' *> pure Dot
  , Name <$> parseName
  ] <* P.skipSpaces

 where
  parseName :: P.Parser String String
  parseName = fromCharArray <$> do
    A.cons
      <$> identStart
      <*> A.many identLetter

  identStart :: P.Parser String Char
  identStart = alpha

  identLetter :: P.Parser String Char
  identLetter = alphaNum <|> P.oneOf ['_', '-']

type TokenParser a = P.Parser (List PositionedToken) a

-- token :: PositionedToken -> TokenParser Unit
-- token (PositionedToken { sourcePos=pos }) =
--   P.ParserT $ \(P.PState { input=toks }) ->
--     return $ case toks of
--       Cons x xs -> { consumed: true
--                    , input: xs
--                    , result: Right x
--                    , position: pos }
--       _ -> P.fail "expected token, met EOF"