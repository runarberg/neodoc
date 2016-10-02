module Neodoc.Spec.Lexer2 where

import Prelude
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Data.List as L
import Data.Monoid (mempty)
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
import Neodoc.Spec.Error (SpecParseError(..))

import Neodoc.Spec.ParserState (ParserState(..))
import Neodoc.Spec.ParserState as ParserState
import Neodoc.Spec.Token

import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.Combinators as P
import Text.Parsing.StringParser.String as P

lex :: Mode -> String -> Either SpecParseError (List PositionedToken)
lex m input = lmap (SpecParseError <<< getParseErrorMessage) $
  -- perform a simple transformation to avoid 'manyTill' and safe some millis
  -- lexing. Hopefully this won't be necessary when purescript-parsing improves
  -- performance, a faster parsing library shows up or the purescript compiler
  -- improves in performance.
  let input' = case m of
                Usage        -> Regex.replace referenceRegex "@$1" input
                Descriptions -> input
   in P.runParser (parseTokens m) input'

  where getParseErrorMessage (P.ParseError m) = m

parseTokens :: Mode -> P.Parser (L.List PositionedToken)
parseTokens m =
  let tokParser = case m of
                    Usage        -> parseUsageToken
                    Descriptions -> parseDescriptionToken
   in do
    skipSpaces
    xs <- P.many $ parsePositionedToken tokParser
    P.eof <|> void do
      i <- getInput
      P.fail $ "Unexpected input: " <> i
    pure xs

lexDescs :: String -> Either SpecParseError (List PositionedToken)
lexDescs = lex Descriptions

lexUsage :: String -> Either SpecParseError (List PositionedToken)
lexUsage = lex Usage

data Mode = Usage | Descriptions

instance showMode :: Show Mode where
  show (Usage)        = "Usage"
  show (Descriptions) = "Descriptions"

data PositionedToken = PositionedToken P.Pos Token

parsePositionedToken :: P.Parser Token -> P.Parser PositionedToken
parsePositionedToken p = PositionedToken <$> getPosition <*> p
  where
  getPosition = P.Parser \{ str, pos } -> Right {
    suffix: { str, pos }
  , result: pos
  }

getInput :: P.Parser String
getInput = P.Parser \{ str, pos } -> Right {
    suffix: { str, pos }
  , result: str
  }

parseUsageToken :: P.Parser Token
parseUsageToken = P.choice [
    P.char   '('   $> LParen
  , P.char   ')'   $> RParen
  , P.char   ']'   $> RSquare
  , P.char   '|'   $> VBar
  , P.char   ':'   $> Colon
  , P.char   ','   $> Comma
  , P.string "..." $> TripleDot
  , P.char   '['   $> LSquare
  , _reference
  , P.try _longOption
  , P.try _shortOption
  , P.try _eoa
  , _stdin
  , AngleName <$> _angleName
  , maybeShoutName
  ]
  <* skipSpaces -- skip spaces *AND* newlines

parseDescriptionToken :: P.Parser Token
parseDescriptionToken = P.choice [
    P.char   ','   $> Comma
  , P.char   '('   $> LParen
  , P.char   ')'   $> RParen
  , P.char   ']'   $> RSquare
  , P.string "..." $> TripleDot
  , P.try _longOption
  , P.try _shortOption
  , AngleName <$> _angleName
  , maybeShoutName
  , P.try _tag
  , P.char '[' $> LSquare
  , _reference
  , eol $> Newline
  , Garbage <$> P.anyChar
  ]
  <* spaces -- skip only spaces ' ' and '\t'

notFollowedBy :: ∀ a. P.Parser a -> P.Parser Unit
notFollowedBy p = P.try $ (P.try p *> P.fail "Negated parser succeeded") <|> pure unit

white :: P.Parser Unit
white = void $ P.oneOf [ '\n', '\r', ' ', '\t' ]

identLetter :: P.Parser Char
identLetter = P.alphaNum <|> P.oneOf ['_', '-']

maybeShoutNameRegex :: Regex
maybeShoutNameRegex
  = unsafePartial $ fromRight $
      regex "[a-zA-Z]" (Regex.parseFlags "gi")

maybeShoutName :: P.Parser Token
maybeShoutName = do
  n <- _anyName
  pure if (String.toUpper n == n && Regex.test maybeShoutNameRegex n)
          then ShoutName n
          else Name n

_anyName :: P.Parser String
_anyName = do
  foldMap String.singleton <$> do
    (:)
      <$> P.alphaNum
      <*> P.many do
            P.choice $ P.try <$> [
              identLetter
            , P.char '.' <* (notFollowedBy $ P.string "..")
            , P.oneOf [ '-', '_' ]
          ]

_stdin :: P.Parser Token
_stdin = do
  P.char '-'
  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char '|'
  , void $ P.char ']'
  , void $ P.char ')'
  , void $ P.string "..."
  ])
  pure Dash

_eoa :: P.Parser Token
_eoa = do
  P.string "--"
  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char ']'
  , void $ P.char ')'
  ])
  pure DoubleDash

_reference :: P.Parser Token
_reference = Reference <$> do
  P.char '@'
  foldMap String.singleton <$> P.many (P.noneOf [' ', '\n'])

_tag :: P.Parser Token
_tag = P.between (P.char '[') (P.char ']') do
  s <- trim <<< foldMap String.singleton <$> P.many1 (P.noneOf [']'])
  case A.uncons (String.split ":" s) of
    Nothing -> P.fail "Expected label"
    Just { head: _, tail: xs } | A.length xs == 0 ->  P.fail "Expected label"
    Just { head: x, tail: xs } ->
      let v = trim (String.joinWith ":" xs)
       in pure (Tag x v)

_angleName :: P.Parser String
_angleName = do
  P.char '<'
  n <- foldMap String.singleton <$> do
    P.many1 $ P.choice [
      identLetter
      -- disallow swallowing new `<`s in order to avoid creating hard to trace
      -- errors for the user
    , P.noneOf [ '<', '>' ]
    ]
  P.char '>'
  pure $ "<" <> n <> ">"

_shortOption :: P.Parser Token
_shortOption = do
  let validChar = P.alphaNum <|> P.oneOf [ '?' ]

  P.char '-'
  x  <- validChar
  xs <- P.many validChar

  arg <- P.option Nothing $ P.choice [

    -- Case 1: -foo=BAR
    Just <$> do
      P.char '='
      n <- P.choice [ _angleName, _anyName ]
      pure  { name:     n
            , optional: false
            }

    -- Case 2: Option[=ARG]
  , Just <$> do
      P.char '['
      P.optional $ P.char '='
      n <- P.choice [ _angleName, _anyName ]
      P.char ']'
      pure  { name:     n
            , optional: true
            }

    -- Case 3: Option<ARG>
  , Just <$> do
      n <- _angleName
      pure { name:     n
            , optional: false
            }
  ]

  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char '|'
  , void $ P.string "..."
  , void $ P.char ']'
  , void $ P.char ')'
  , void $ P.char ',' -- desc mode only
  ])

  pure $ SOpt (x:|A.fromFoldable xs) arg

_longOption :: P.Parser Token
_longOption = do
  P.string "--"

  name' <- foldMap String.singleton <$> do
    (:)
      <$> P.alphaNum
      <*> (P.many $ P.choice [
            P.alphaNum
          , P.oneOf [ '-' ] <* P.lookAhead P.alphaNum
          ])

  arg <- P.option Nothing $ P.choice [

    -- Case 1: OPTION=ARG
    Just <$> do
      P.char '='
      n <- P.choice [ _angleName, _anyName ]
      pure  { name: n
            , optional: false
            }

    -- Case 2: Option[=ARG]
  , Just <$> do
      P.char '['
      P.optional $ P.char '='
      n <- P.choice [ _angleName, _anyName ]
      P.char ']'
      pure  { name:     n
            , optional: true
            }
  ]

  -- Ensure the argument is correctly bounded
  P.eof <|> (P.lookAhead $ P.choice [
    void $ white
  , void $ P.char '|'
  , void $ P.string "..."
  , void $ P.char ']'
  , void $ P.char ')'
  , void $ P.char ',' -- desc mode only
  ])

  pure $ LOpt name' arg

space :: P.Parser Char
space = P.satisfy \c -> c == ' ' || c == '\t'

eol :: P.Parser Unit
eol = (void $ P.string "\r\n") <|> (void $ P.char '\n')

spaces :: P.Parser _
spaces = P.many space

-- | Optimal: Faster P.skipSpaces since it does not accumulate into a list.
skipSpaces = go
  where
    go = (do
      P.satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
      go
    ) <|> pure unit

-- | Optimal: Translate [[<anything>-]options] to @anything
-- | this saves us looking ahead repeatedly when parsing '['.
referenceRegex :: Regex
referenceRegex
  = unsafePartial $ fromRight $
      regex
        "\\[(([^\\]](?!\\s*-?\\s*options\\s*))*?.?)\\s*-?\\s*options\\s*(\\.\\.\\.)?\\s*\\]"
        (Regex.parseFlags "gmi")


