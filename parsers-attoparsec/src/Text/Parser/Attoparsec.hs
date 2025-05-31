{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Attoparsec
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Attoparsec instances for the parsers package
--
-----------------------------------------------------------------------------
module Text.Parser.Attoparsec (ParsersAttoparsec(..)) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import qualified Data.Attoparsec.Types as Att
import qualified Data.Attoparsec.Combinator as Att
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token (TokenParsing(..))

newtype ParsersAttoparsec t a = ParsersAttoparsec { getParsersAttoparsec :: Att.Parser t a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Att.Chunk t => Parsing (ParsersAttoparsec t) where
  try (ParsersAttoparsec p) = ParsersAttoparsec (Att.try p)
  ParsersAttoparsec p <?> l = ParsersAttoparsec (p Att.<?> l)
  skipMany (ParsersAttoparsec p) = ParsersAttoparsec (Att.skipMany p)
  skipSome (ParsersAttoparsec p) = ParsersAttoparsec (Att.skipMany1 p)
  unexpected = ParsersAttoparsec . fail
  eof = ParsersAttoparsec Att.endOfInput
  notFollowedBy p = optional p >>= maybe (pure ()) (unexpected . show)

instance Att.Chunk t => CharParsing (ParsersAttoparsec t) where
  satisfy p = ParsersAttoparsec (fmap e2c $ Att.satisfyElem $ p . e2c)
    where e2c = Att.chunkElemToChar (undefined :: t)

instance Att.Chunk t => LookAheadParsing (ParsersAttoparsec t) where
  lookAhead (ParsersAttoparsec p) = ParsersAttoparsec (Att.lookAhead p)

instance Att.Chunk t => TokenParsing (ParsersAttoparsec t)
