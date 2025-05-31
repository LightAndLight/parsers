{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Parsec
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsec instances for the parsers package
--
-----------------------------------------------------------------------------
module Text.Parser.Parsec (ParsersParsecT(..)) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.State.Class as Class
import Control.Monad.Reader.Class as Class
import qualified Text.Parsec as P
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token (TokenParsing(..))

newtype ParsersParsecT s u m a = ParsersParsecT { getParsersParsecT :: P.ParsecT s u m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans, MonadState s, MonadReader r)

instance (P.Stream s m t, Show t) => Parsing (ParsersParsecT s u m) where
  try = ParsersParsecT . P.try . getParsersParsecT
  ParsersParsecT p <?> l = ParsersParsecT (p P.<?> l)
  skipMany = ParsersParsecT . P.skipMany . getParsersParsecT
  skipSome = ParsersParsecT . P.skipMany1 . getParsersParsecT
  unexpected = ParsersParsecT . P.unexpected
  eof = ParsersParsecT P.eof
  notFollowedBy (ParsersParsecT p) = ParsersParsecT (P.notFollowedBy p)

instance P.Stream s m Char => CharParsing (ParsersParsecT s u m) where
  satisfy = ParsersParsecT . P.satisfy
  char = ParsersParsecT . P.char
  notChar c = ParsersParsecT (P.satisfy (/= c))
  anyChar = ParsersParsecT P.anyChar
  string = ParsersParsecT . P.string

instance (P.Stream s m t, Show t) => LookAheadParsing (ParsersParsecT s u m) where
  lookAhead (ParsersParsecT p) = ParsersParsecT (P.lookAhead p)

instance P.Stream s m Char => TokenParsing (ParsersParsecT s u m)
