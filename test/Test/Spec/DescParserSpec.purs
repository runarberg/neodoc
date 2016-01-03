module Test.Spec.DescParserSpec (descParserSpec) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Debug.Trace
import Data.List (fromList)
import Data.Either (Either(..), either)
import Control.Bind ((=<<))
import Data.Maybe (Maybe(..))
import Data.Foldable (for_)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Text.Parsing.Parser as P

import Docopt
import qualified Docopt.Spec.Parser.Desc as Desc
import qualified Docopt.Spec.Parser.Lexer as Lexer
import Docopt.Spec.Parser.Base (debug)
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

newtype TestCase = TestCase { input :: String
                            , output :: Either String (Array Desc.Desc) }

pass :: String -> Array Desc.Desc -> TestCase
pass input output = TestCase { input: input, output: Right output }

fail :: String -> String -> TestCase
fail input msg = TestCase { input: input, output: Left msg }

o = Desc.Option

descParserSpec =
  describe "description parser" do
    for_ [ pass
              "-f, --foo"
            [ o { flag:    Just 'f'
                , long:    Just "foo"
                , arg:     Nothing
                , default: Nothing }
            ]
          ]
          runtest

  where
    runtest (TestCase { input=input, output=output }) = do
      it (input ++ " " ++
        (either (\msg -> "should fail with " ++ show msg)
                (\out -> "should succeed with " ++
                            (show $ Desc.prettyPrintDesc <$> out))
                output)) do
        vliftEff $ evaltest (Desc.parse =<< Lexer.lex input) output

    evaltest (Left (P.ParseError { message: msg })) (Left msg')
      = if msg == msg'
           then return unit
           else throwException $ error $ "Unexpected error:\n" ++ msg
    evaltest (Left e) _ = throwException $ error $ show e

    evaltest (Right out) (Left _)
      = throwException $ error $
          "Missing exception! Got: " ++ (show $ Desc.prettyPrintDesc <$> out)

    evaltest (Right out) (Right expected)
      = let out' = fromList out
         in if (out' == expected)
              then return unit
              else throwException $ error $
                    "Unexpected output:" ++ (show $ Desc.prettyPrintDesc <$> out')
