{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Binary
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Binary instances for the parsers package
--
-----------------------------------------------------------------------------
module Text.Parser.Binary (ParsersBinary(..)) where

import Control.Applicative
import Control.Monad (MonadPlus(..), when, unless)
import qualified Data.Binary.Get as B
import Text.Parser.Combinators
import Text.Parser.LookAhead

newtype ParsersBinary a = ParsersBinary { getParsersBinary :: B.Get a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Parsing ParsersBinary where
  try = id
  (<?>) (ParsersBinary p) label = ParsersBinary $ B.label label p
  skipMany p = ParsersBinary $ do 
    skipped <- True <$ getParsersBinary p <|> pure False
    when skipped $ getParsersBinary (skipMany p)
  unexpected = ParsersBinary . fail
  eof = ParsersBinary $ do 
    isEmpty <- B.isEmpty
    unless isEmpty $ fail "Parsing.eof"
  notFollowedBy p = optional p >>= maybe (pure ()) (unexpected . show)

instance LookAheadParsing ParsersBinary where
  lookAhead (ParsersBinary p) = ParsersBinary (B.lookAhead p)
