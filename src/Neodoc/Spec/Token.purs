module Neodoc.Spec.Token where

import Prelude
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Pretty
import Data.Array as A
import Data.String as String
import Data.String (fromCharArray)
import Data.NonEmpty (NonEmpty, (:|))
import Neodoc.Data.OptionArgument

import Text.Parsing.StringParser as P

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | Dash
  | VBar
  | Colon
  | Comma
  | Newline
  | TripleDot
  | Reference String
  | LOpt String (Maybe OptionArgumentObj)
  | SOpt (NonEmpty Array Char) (Maybe OptionArgumentObj)
  | Tag String String
  | Name String
  | ShoutName String
  | AngleName String
  | Garbage Char
  | DoubleDash

type OptionArgumentObj = {
  name     :: String
, optional :: Boolean
}

newtype OptionArgument = OptionArgument OptionArgumentObj

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument o) = "OptionArgument { name: " <> show o.name
       <> ", optional: " <> show o.optional <> "}"

instance prettyToken :: Pretty Token where
  pretty LParen        = String.singleton '('
  pretty RParen        = String.singleton ')'
  pretty LSquare       = String.singleton '['
  pretty RSquare       = String.singleton ']'
  pretty Dash          = String.singleton '-'
  pretty VBar          = String.singleton '|'
  pretty Newline       = String.singleton '\n'
  pretty Colon         = String.singleton ':'
  pretty Comma         = String.singleton ','
  pretty TripleDot     = "..."
  pretty DoubleDash    = "--"
  pretty (Reference r) = "Reference " <> show r
  pretty (Garbage   c) = "Garbage "   <> show c
  pretty (Tag k v)     = "Tag "       <> show k <> " " <> show v
  pretty (Name      n) = "Name "      <> show n
  pretty (ShoutName n) = "ShoutName " <> show n
  pretty (AngleName n) = "AngleName " <> show n
  pretty (LOpt n arg)  = "--" <> n <> arg'
    where arg' = fromMaybe "" do
                  arg <#> \a ->
                    if a.optional then "[" else ""
                      <> a.name
                      <> if a.optional then "]" else ""
  pretty (SOpt (c :| cs) arg) = "-" <> n <> arg'
    where n = fromCharArray $ A.cons c cs
          arg' = fromMaybe "" do
                  arg <#> \a ->
                       (if a.optional then "[" else "")
                    <> a.name
                    <> (if a.optional then "]" else "")

instance showToken :: Show Token where
  show LParen = "LParen"
  show RParen = "RParen"
  show LSquare = "LSquare"
  show RSquare = "RSquare"
  show Dash = "Dash"
  show VBar = "VBar"
  show Colon = "Colon"
  show Comma = "Comma"
  show Newline = "Newline"
  show TripleDot = "TripleDot"
  show (Reference n) = "Reference " <> show n
  show (LOpt n mA) = "LOpt " <> show n <> " " <> show (OptionArgument <$> mA)
  show (SOpt cs mA) = "SOpt " <> show cs <> " " <> show (OptionArgument <$> mA)
  show (Tag x y) = "Tag " <> show x <> " " <> show y
  show (Name n) = "Name " <> show n
  show (ShoutName n) = "ShoutName " <> show n
  show (AngleName n) = "AngleName " <> show n
  show (Garbage c) = "Garbage " <> show c
  show DoubleDash = "DoubleDash"

instance eqToken :: Eq Token where
  eq LParen            LParen             = true
  eq RParen            RParen             = true
  eq LSquare           LSquare            = true
  eq RSquare           RSquare            = true
  eq VBar              VBar               = true
  eq Colon             Colon              = true
  eq Comma             Comma              = true
  eq Dash              Dash               = true
  eq DoubleDash        DoubleDash         = true
  eq TripleDot         TripleDot          = true
  eq Newline           Newline            = true
  eq (Reference r)     (Reference r')     = r == r'
  eq (LOpt n arg)      (LOpt n' arg')
    = (n == n')
    && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              pure $ (a.name == a'.name)
                  && (a.optional == a'.optional)
            ))
  eq (SOpt (c:|cs) arg) (SOpt (c':|cs') arg')
    = (c == c') && (cs == cs')
    && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              pure $ (a.name == a'.name)
                  && (a.optional == a'.optional)
            ))
  eq (AngleName n)     (AngleName n')     = n == n'
  eq (ShoutName n)     (ShoutName n')     = n == n'
  eq (Name n)          (Name n')          = n == n'
  eq (Garbage c)       (Garbage c')       = c == c'
  eq _ _                                  = false
