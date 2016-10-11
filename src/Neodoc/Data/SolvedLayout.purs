module Neodoc.Data.SolvedLayout where

import Prelude
import Data.Generic
import Data.Either (Either(..))
import Data.Pretty (class Pretty, pretty)
import Data.Maybe (Maybe(..), maybe)
import Data.Bifunctor (lmap)
import Data.Tuple.Nested ((/\))
import Data.Foldable (intercalate, all)
import Data.String as String
import Data.List (List)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Neodoc.Data.Layout
import Neodoc.OptionAlias
import Neodoc.OptionAlias as OptionAlias
import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (class ToArgKey)
import Neodoc.Data.OptionArgument
import Data.Function (on)

-- This type can be specialized for elements of a usage section
type SolvedLayout = Layout SolvedLayoutArg
data SolvedLayoutArg
  = Command     String Boolean
  | Positional  String Boolean
  | Option      OptionAlias (Maybe OptionArgument) Boolean
  | EOA
  | Stdin

derive instance eqSolvedLayoutArg :: Eq SolvedLayoutArg
derive instance ordSolvedLayoutArg :: Ord SolvedLayoutArg
derive instance genericSolvedLayoutArg :: Generic SolvedLayoutArg

instance showSolvedLayoutArg :: Show SolvedLayoutArg where
  show = gShow

instance toArgKeySolvedLayoutArg :: ToArgKey SolvedLayoutArg where
  toArgKey (Command     n   _) = CommandKey n
  toArgKey (Positional  n   _) = PositionalKey n
  toArgKey (Option      a _ _) = OptionKey a
  toArgKey (EOA              ) = EOAKey
  toArgKey (Stdin            ) = StdinKey

instance prettySolvedLayoutArg :: Pretty SolvedLayoutArg where
  pretty = go
    where
    go (Command n r) = n <> rep r
    go (Positional n r) = n <> rep r
    go EOA = "--"
    go Stdin = "-"
    go (Option a mA r) = pretty a <> maybe "" (prettyOA a) mA <> rep r
    rep r = if r then "..." else ""
    prettyOA a (OptionArgument n o)
      = (if o then "[=" else (if OptionAlias.isLong a then "=" else ""))
        <> n
        <> (if o then "]" else "")

instance asForeignSolvedLayoutArg :: AsForeign SolvedLayoutArg where
  write (Command n r) = F.toForeign {
      type: "Command"
    , name: F.write n
    , repeatable: F.write r
    }
  write (Positional n r) = F.toForeign {
      type: "Positional"
    , name: F.write n
    , repeatable: F.write r
    }
  write (Option n mArg r) = F.toForeign {
      type: "Option"
    , name: F.write n
    , argument: maybe F.undefined F.write mArg
    , repeatable: F.write r
    }
  write Stdin = F.toForeign { type: "Stdin" }
  write EOA = F.toForeign { type: "EOA" }

instance isForeignSolvedLayoutArg :: IsForeign SolvedLayoutArg where
  read v = do
    typ :: String <- String.toUpper <$> F.readProp "type" v

    case typ of
      "EOA" -> pure EOA
      "STDIN" -> pure Stdin
      "COMMAND" ->
        Command
          <$> F.readProp "name"       v
          <*> F.readProp "repeatable" v
      "POSITIONAL" ->
        Positional
          <$> F.readProp "name"       v
          <*> F.readProp "repeatable" v
      "OPTION" ->
        Option
          <$> F.readProp "name" v
          <*> F.readPropMaybe "argument" v
          <*> F.readProp "repeatable" v
      _ -> Left $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

isRepeatable :: SolvedLayout -> Boolean
isRepeatable (Group           _ r _) = r
isRepeatable (Elem (Command    _ r)) = r
isRepeatable (Elem (Positional _ r)) = r
isRepeatable (Elem (Option   _ _ r)) = r
isRepeatable _ = false

setRepeatable :: Boolean -> SolvedLayout -> SolvedLayout
setRepeatable r (Group o _ xs) = Group o r xs
setRepeatable r (Elem       x) = Elem $ setElemRepeatable r x

setElemRepeatable :: Boolean -> SolvedLayoutArg -> SolvedLayoutArg
setElemRepeatable r (Command     n _) = Command n r
setElemRepeatable r (Positional  n _) = Positional n r
setElemRepeatable r (Option   a mA _) = Option a mA r
setElemRepeatable _ x = x

setRepeatableOr :: Boolean -> SolvedLayout -> SolvedLayout
setRepeatableOr r (Group           o r' xs) = Group o (r || r') xs
setRepeatableOr r (Elem (Command     n r')) = Elem (Command n (r || r'))
setRepeatableOr r (Elem (Positional  n r')) = Elem (Positional n (r || r'))
setRepeatableOr r (Elem (Option   a mA r')) = Elem (Option a mA (r || r'))
setRepeatableOr _ x = x

isOptional :: SolvedLayout -> Boolean
isOptional (Group o _ _) = o
isOptional _ = false

isPositional :: SolvedLayoutArg -> Boolean
isPositional (Positional _ _) = true
isPositional _ = false

isCommand :: SolvedLayoutArg -> Boolean
isCommand (Command _ _) = true
isCommand _ = false

isOptionElem :: SolvedLayout -> Boolean
isOptionElem (Elem x) = isOption x
isOptionElem _ = false

isOption :: SolvedLayoutArg -> Boolean
isOption (Option _ _ _) = true
isOption EOA = true
isOption Stdin = true
isOption _ = false

isFlag :: SolvedLayoutArg -> Boolean
isFlag (Option _ Nothing _) = true
isFlag _ = false

isGroup :: SolvedLayout -> Boolean
isGroup (Group _ _ _) = true
isGroup _ = false

-- Is this layout considered "free"?
isFreeLayout :: SolvedLayout -> Boolean
isFreeLayout (Elem (Option _ _ _)) = true
isFreeLayout (Elem EOA) = true
isFreeLayout (Elem Stdin) = true
isFreeLayout (Elem _) = false
isFreeLayout (Group _ _ xs) = all (all isFreeLayout) xs
