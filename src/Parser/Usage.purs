module Docopt.Parser.Usage where

import Prelude
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Control.Monad.State (get)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.List (List(..), many, some, (:), toList, concat)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import qualified Data.List as L
import Data.String (length)
import Data.Either
import Data.Tuple
import Data.Maybe hiding (maybe)
import Docopt.Parser.Base
import Docopt.Parser.Common
import Docopt.Parser.Lexer
import Docopt.Parser.State

type OptionAlias    = String
type OptionArgument = String
type IsOptional     = Boolean
type IsRepeatable   = Boolean

-- | Represent a single program usage.
-- | A single usage is made up of a list of mutually exclusive groups,
-- | separated by a vertical bar `|`. Each of those groups can contain
-- | one or more `UsageNode`.
-- |
-- | node node | node | node
-- | ^^^^ ^^^^   ^^^^   ^^^^
-- |   |   |      |      |
-- | [ 0 , 1 ]  [ 0 ]  [ 0 ]
-- |    \ /       |      |
-- | [   0    ,   1   ,  2 ]
data Usage = Usage String (List (List UsageNode))
data UsageNode
  = Command     String
  | Positional  String
                IsRepeatable
  | Option      String
                (Maybe OptionArgument)
                IsRepeatable
  | OptionStack Char
                (List Char)
                (Maybe OptionArgument)
                IsRepeatable
  | Group       IsOptional
                (List (List UsageNode))
                IsRepeatable

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

instance showUsageNode :: Show UsageNode where
  show (Command n) =
    "Command " ++ n
  show (Positional n b) =
    "Positional " ++ n ++ " " ++ show b
  show (Option n a b) =
    "Option " ++ show n ++ " " ++ show a ++ " " ++ show b
  show (OptionStack n s a b) =
    "OptionStack " ++ show n ++ " " ++ show s ++ " " ++ show a ++ " " ++ show b
  show (Group n b o) =
    "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

-- | Parse the usage section
parseUsage :: TokenParser (List Usage)
parseUsage = do

  -- Calculate the leading start indentation.
  -- The first usage line is indicative of the
  -- start indentation of every other usage line!
  name <- parseProgram
  col' <- getCol
  let startCol = col' - (length name) - 1

  markIndent' startCol $ P.manyTill
    (P.try $ Usage name <$> (parseElems name))
    eof

  where

    parseProgram :: TokenParser String
    parseProgram = name

    parseElems :: String -> TokenParser (List (List UsageNode))
    parseElems programName = do
      concat <$> P.manyTill
        ((some parseElem) `P.sepBy1` vbar)
        (matchProgram <|> (P.lookAhead eof))

      where

        matchProgram :: TokenParser Unit
        matchProgram = do
          sameIndent
          s <- parseProgram
          guard (s == programName) P.<?> "Program token " ++ s

        parseElem :: TokenParser UsageNode
        parseElem = defer \_ -> do
          P.notFollowedBy matchProgram
          P.choice $ map (indented *>)
            [ parseOption
            , parsePositional
            , parseCommand
            , parseGroup
            ] P.<?> "Option, Positional, Command or Group"

        parseLongOption :: TokenParser UsageNode
        parseLongOption = Option
            <$> lopt
            <*> (tryMaybe do
                  equal
                  (shoutName <|> angleName <|> name))
            <*> parseRepetition

        parseShortOption :: TokenParser UsageNode
        parseShortOption = do
            { flag: flag, stack: stack, arg: arg } <- sopt
            OptionStack flag stack
                <$> (case arg of
                      Just _  -> pure $ arg
                      Nothing -> do
                        (tryMaybe do
                          equal
                          (shoutName <|> angleName <|> name)))
                <*> parseRepetition

        parseOption :: TokenParser UsageNode
        parseOption = (parseLongOption <|> parseShortOption)

        parsePositional :: TokenParser UsageNode
        parsePositional = Positional
          <$> (angleName <|> shoutName)
          <*> parseRepetition

        parseCommand :: TokenParser UsageNode
        parseCommand = Command <$> name

        parseGroup :: TokenParser UsageNode
        parseGroup = defer \_ -> P.choice
          [ parseReqGroup
          , parseOptGroup ]

        parseOptGroup :: TokenParser UsageNode
        parseOptGroup = defer \_ -> Group true
          <$> (P.between
                (indented *> lsquare)
                (indented *> rsquare)
                ((some parseElem) `P.sepBy1` vbar))
          <*> parseRepetition

        parseReqGroup :: TokenParser UsageNode
        parseReqGroup = defer \_ -> Group false
          <$> (P.between
                (indented *> lparen)
                (indented *> rparen)
                ((some parseElem) `P.sepBy1` vbar))
          <*> parseRepetition

        parseRepetition :: TokenParser Boolean
        parseRepetition = P.choice
          [ P.try $ indented *> tripleDot *> pure true
          , pure false ]
