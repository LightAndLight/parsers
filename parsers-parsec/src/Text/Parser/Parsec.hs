-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Parsec
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Isaac Elliott <hackage+parsers@ielliott.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- Type class instances for Parsec.
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Parser.Parsec where

import Text.Parser.Char (CharParsing(..))
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.LookAhead (LookAheadParsing(..))
import Text.Parser.Token (TokenParsing(..))
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)

import qualified Text.Parsec as Parsec

newtype Parsec s u m a = Parsec { runParsec :: Parsec.ParsecT s u m a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)

instance Parsec.Stream s m Char => CharParsing (Parsec s u m) where
  satisfy   = Parsec . Parsec.satisfy
  char      = Parsec . Parsec.char
  notChar c = Parsec $ Parsec.satisfy (/= c)
  anyChar   = Parsec Parsec.anyChar
  string    = Parsec . Parsec.string

instance (Parsec.Stream s m t, Show t) => Parsing (Parsec s u m) where
  try           = Parsec . Parsec.try . runParsec
  (<?>) p s     = Parsec (runParsec p Parsec.<?> s)
  skipMany      = Parsec . Parsec.skipMany . runParsec
  skipSome      = Parsec . Parsec.skipMany1 . runParsec
  unexpected    = Parsec . Parsec.unexpected
  eof           = Parsec Parsec.eof
  notFollowedBy = Parsec . Parsec.notFollowedBy . runParsec

instance (Parsec.Stream s m t, Show t) => LookAheadParsing (Parsec s u m) where
  lookAhead = Parsec . Parsec.lookAhead . runParsec

instance Parsec.Stream s m Char => TokenParsing (Parsec s u m)
