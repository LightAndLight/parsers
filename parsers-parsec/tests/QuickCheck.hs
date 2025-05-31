{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Main
( main
) where

import Control.Applicative
import Data.Either

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Parsec
import qualified Text.Parsec as P

import System.Exit

-- -------------------------------------------------------------------------- --
-- Run tests with the Parsec parser

newtype P a = P (forall m. (Monad m, CharParsing m) => m a)

data TestParser a = TestParser String (P a -> String -> Either String a)

instance Show (TestParser a) where show (TestParser n _) = n

pParsec :: TestParser a
pParsec = TestParser "Parsec" $ \(P p) -> either (Left . show) Right . P.parse (getParsersParsecT p) "test input"

instance Arbitrary (TestParser a) where
    arbitrary = elements [pParsec]

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = mapM quickCheckResult tests >>= \x -> case filter (not . passed) x of
    [] -> exitSuccess
    _ -> exitFailure
  where
    passed Success{} = True
    passed _ = False

-- -------------------------------------------------------------------------- --
-- Tests

tests :: [Property]
tests =
    [ property prop_notFollowedBy0
    , property prop_notFollowedBy1
    , property prop_notFollowedBy2
    , property prop_notFollowedBy3
    ]

-- -------------------------------------------------------------------------- --
-- Properties

prop_notFollowedBy0 :: TestParser Char -> Char -> Char -> Bool
prop_notFollowedBy0 (TestParser _ p) x y = either (\_ -> x == y) (/= y)
    $ p (P (notFollowedBy (char y) *> anyChar)) [x]

prop_notFollowedBy1 :: TestParser Char -> Char -> Bool
prop_notFollowedBy1 (TestParser _ p) x = either (\_ -> x == x) (/= x)
    $ p (P (notFollowedBy (char x) *> anyChar)) [x]

prop_notFollowedBy2 :: TestParser Char -> String -> Char -> Bool
prop_notFollowedBy2 (TestParser _ p) x y = isLeft
    $ p (P (anyChar *> notFollowedBy (char y) *> char y)) x

prop_notFollowedBy3 :: TestParser () -> Char -> Bool
prop_notFollowedBy3 (TestParser _ p) x = isRight
    $ p (P (notFollowedBy (char x) <|> char x *> pure ())) [x]
