-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Attoparsec
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Isaac Elliott <hackage+parsers@ielliott.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- Type class instances for Attoparsec.
--
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Parser.Attoparsec where

import Control.Applicative (optional, Alternative)
import Control.Monad (MonadPlus)
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.Char (CharParsing(..))
import Text.Parser.LookAhead (LookAheadParsing(..))
import Text.Parser.Token (TokenParsing(..))

import qualified Data.Attoparsec.Types as Att
import qualified Data.Attoparsec.Combinator as Att

newtype Attoparsec t a = Attoparsec { runAttoparsec :: Att.Parser t a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)

instance Att.Chunk t => Parsing (Attoparsec t) where
  try             = Attoparsec . Att.try . runAttoparsec
  (<?>) p s       = Attoparsec (runAttoparsec p Att.<?> s)
  skipMany        = Att.skipMany
  skipSome        = Att.skipMany1
  unexpected      = fail
  eof             = Attoparsec Att.endOfInput
  notFollowedBy p = optional p >>= maybe (pure ()) (unexpected . show)

instance Att.Chunk t => CharParsing (Attoparsec t) where
  satisfy p = Attoparsec . fmap e2c $ Att.satisfyElem $ p . e2c
    where e2c = Att.chunkElemToChar (undefined :: t)
  {-# INLINE satisfy #-}

instance Att.Chunk i => LookAheadParsing (Attoparsec i) where
  lookAhead = Attoparsec . Att.lookAhead . runAttoparsec

instance Att.Chunk t => TokenParsing (Attoparsec t)
