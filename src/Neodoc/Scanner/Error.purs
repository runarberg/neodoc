module Neodoc.Scanner.Error where

import Data.Pretty (class Pretty, pretty)
import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError)
import Text.Parsing.Parser as P

newtype ScanError = ScanError P.ParseError

instance toNeodocErrorScanError :: ToNeodocError ScanError where
  toNeodocError (ScanError (P.ParseError m _ _)) = Neodoc.ScanError m

instance prettyScanError :: Pretty ScanError where
  pretty (ScanError (P.ParseError m _ _)) = m
