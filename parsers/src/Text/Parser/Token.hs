{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fspec-constr -fspec-constr-count=8 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Token
-- Copyright   :  (c) Edward Kmett 2011
--                (c) Daan Leijen 1999-2001
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers that comprehend whitespace and identifier styles
--
-- > idStyle    = haskellIdents { styleReserved = ... }
-- > identifier = ident idStyle
-- > reserved   = reserve idStyle
--
-----------------------------------------------------------------------------
module Text.Parser.Token
  (
  -- * Token Parsing
    whiteSpace      -- :: TokenParsing m => m ()
  , charLiteral     -- :: TokenParsing m => m Char
  , stringLiteral   -- :: (TokenParsing m, IsString s) => m s
  , stringLiteral'  -- :: (TokenParsing m, IsString s) => m s
  , natural         -- :: TokenParsing m => m Integer
  , integer         -- :: TokenParsing m => m Integer
  , double          -- :: TokenParsing m => m Double
  , naturalOrDouble -- :: TokenParsing m => m (Either Integer Double)
  , integerOrDouble -- :: TokenParsing m => m (Either Integer Double)
  , scientific      -- :: TokenParsing m => m Scientific
  , naturalOrScientific -- :: TokenParsing m => m (Either Integer Scientific)
  , integerOrScientific -- :: TokenParsing m => m (Either Integer Scientific)
  , symbol          -- :: TokenParsing m => String -> m String
  , textSymbol      -- :: TokenParsing m => Text -> m Text
  , symbolic        -- :: TokenParsing m => Char -> m Char
  , parens          -- :: TokenParsing m => m a -> m a
  , braces          -- :: TokenParsing m => m a -> m a
  , angles          -- :: TokenParsing m => m a -> m a
  , brackets        -- :: TokenParsing m => m a -> m a
  , comma           -- :: TokenParsing m => m Char
  , colon           -- :: TokenParsing m => m Char
  , dot             -- :: TokenParsing m => m Char
  , semiSep         -- :: TokenParsing m => m a -> m [a]
  , semiSep1        -- :: TokenParsing m => m a -> m [a]
  , commaSep        -- :: TokenParsing m => m a -> m [a]
  , commaSep1       -- :: TokenParsing m => m a -> m [a]
  -- ** Token Parsing Class
  , TokenParsing(..)
  -- ** Token Parsing Transformers
  , Unspaced(..)
  , Unlined(..)
  , Unhighlighted(..)
  -- ** /Non-Token/ Parsers
  , decimal       -- :: TokenParsing m => m Integer
  , hexadecimal   -- :: TokenParsing m => m Integer
  , octal         -- :: TokenParsing m => m Integer
  , characterChar -- :: TokenParsing m => m Char
  , integer'      -- :: TokenParsing m => m Integer
  -- * Identifiers
  , IdentifierStyle(..)
  , liftIdentifierStyle -- :: (MonadTrans t, Monad m) => IdentifierStyle m -> IdentifierStyle (t m)
  , ident           -- :: (TokenParsing m, IsString s) => IdentifierStyle m -> m s
  , reserve         -- :: TokenParsing m => IdentifierStyle m -> String -> m ()
  , reserveText     -- :: TokenParsing m => IdentifierStyle m -> Text -> m ()
  -- ** Lenses and Traversals
  , styleName
  , styleStart
  , styleLetter
  , styleChars
  , styleReserved
  , styleHighlight
  , styleReservedHighlight
  , styleHighlights
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.State.Class as Class
import Control.Monad.Reader.Class as Class
import Control.Monad.Writer.Class as Class
import Data.Char
import Data.Functor.Identity
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.List as List (foldl', transpose)
import Data.Scientific ( Scientific )
import qualified Data.Scientific as Sci
import Data.String
import Data.Text (Text)
import Numeric (showIntAtBase)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token.Highlight


-- | Skip zero or more bytes worth of white space. More complex parsers are
-- free to consider comments as white space.
whiteSpace :: TokenParsing m => m ()
whiteSpace = someSpace <|> pure ()
{-# INLINE whiteSpace #-}

-- | This token parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely).
charLiteral :: forall m. TokenParsing m => m Char
charLiteral = token (highlight CharLiteral lit) where
  lit :: m Char
  lit = between (char '\'') (char '\'' <?> "end of character") characterChar
    <?> "character"
{-# INLINE charLiteral #-}

-- | This token parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
stringLiteral :: forall m s. (TokenParsing m, IsString s) => m s
stringLiteral = fromString <$> token (highlight StringLiteral lit) where
  lit :: m [Char]
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "string"

  stringChar :: m (Maybe Char)
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"

  stringLetter :: m Char
  stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape :: m (Maybe Char)
  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc :: m (Maybe Char)
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode

  escapeEmpty, escapeGap :: m Char
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteral #-}

-- | This token parser behaves as 'stringLiteral', but for single-quoted
-- strings.
stringLiteral' :: forall m s. (TokenParsing m, IsString s) => m s
stringLiteral' = fromString <$> token (highlight StringLiteral lit) where
  lit :: m [Char]
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '\'') (char '\'' <?> "end of string") (many stringChar)
    <?> "string"

  stringChar :: m (Maybe Char)
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"

  stringLetter :: m Char
  stringLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

  stringEscape :: m (Maybe Char)
  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc :: m (Maybe Char)
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode

  escapeEmpty, escapeGap :: m Char
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteral' #-}

-- | This token parser parses a natural number (a non-negative whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
natural :: TokenParsing m => m Integer
natural = token natural'
{-# INLINE natural #-}

-- | This token parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
integer :: forall m. TokenParsing m => m Integer
integer = token (token (highlight Operator sgn <*> natural')) <?> "integer"
  where
  sgn :: m (Integer -> Integer)
  sgn = negate <$ char '-'
    <|> id <$ char '+'
    <|> pure id
{-# INLINE integer #-}

-- | This token parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
double :: TokenParsing m => m Double
double = token (highlight Number (Sci.toRealFloat <$> floating) <?> "double")
{-# INLINE double #-}

-- | This token parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
naturalOrDouble :: TokenParsing m => m (Either Integer Double)
naturalOrDouble = fmap Sci.toRealFloat <$> naturalOrScientific
{-# INLINE naturalOrDouble #-}

-- | This token parser is like 'naturalOrDouble', but handles
-- leading @-@ or @+@.
integerOrDouble :: TokenParsing m => m (Either Integer Double)
integerOrDouble = fmap Sci.toRealFloat <$> integerOrScientific
{-# INLINE integerOrDouble #-}

-- | This token parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
scientific :: TokenParsing m => m Scientific
scientific = token (highlight Number floating <?> "scientific")
{-# INLINE scientific #-}

-- | This token parser parses either 'natural' or a 'scientific'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
naturalOrScientific :: TokenParsing m => m (Either Integer Scientific)
naturalOrScientific = token (highlight Number natFloating <?> "number")
{-# INLINE naturalOrScientific #-}

-- | This token parser is like 'naturalOrScientific', but handles
-- leading @-@ or @+@.
integerOrScientific :: forall m. TokenParsing m => m (Either Integer Scientific)
integerOrScientific = token (highlight Number ios <?> "number")
  where ios :: m (Either Integer Scientific)
        ios = mneg <$> optional (oneOf "+-") <*> natFloating

        mneg (Just '-') nd = either (Left . negate) (Right . negate) nd
        mneg _          nd = nd
{-# INLINE integerOrScientific #-}


-- | Token parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space.
symbol :: TokenParsing m => String -> m String
symbol name = token (highlight Symbol (string name))
{-# INLINE symbol #-}

-- | Token parser @textSymbol t@ parses 'text' @s@ and skips
-- trailing white space.
textSymbol :: TokenParsing m => Text -> m Text
textSymbol name = token (highlight Symbol (text name))
{-# INLINE textSymbol #-}

-- | Token parser @symbolic s@ parses 'char' @s@ and skips
-- trailing white space.
symbolic :: TokenParsing m => Char -> m Char
symbolic name = token (highlight Symbol (char name))
{-# INLINE symbolic #-}

-- | Token parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parens :: TokenParsing m => m a -> m a
parens = nesting . between (symbolic '(') (symbolic ')')
{-# INLINE parens #-}

-- | Token parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@.
braces :: TokenParsing m => m a -> m a
braces = nesting . between (symbolic '{') (symbolic '}')
{-# INLINE braces #-}

-- | Token parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@.
angles :: TokenParsing m => m a -> m a
angles = nesting . between (symbolic '<') (symbolic '>')
{-# INLINE angles #-}

-- | Token parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
brackets :: TokenParsing m => m a -> m a
brackets = nesting . between (symbolic '[') (symbolic ']')
{-# INLINE brackets #-}

-- | Token parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\".
comma :: TokenParsing m => m Char
comma = symbolic ','
{-# INLINE comma #-}

-- | Token parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\".
colon :: TokenParsing m => m Char
colon = symbolic ':'
{-# INLINE colon #-}

-- | Token parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\".
dot :: TokenParsing m => m Char
dot = symbolic '.'
{-# INLINE dot #-}

-- | Token parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep :: TokenParsing m => m a -> m [a]
semiSep p = sepBy p semi
{-# INLINE semiSep #-}

-- | Token parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep1 :: TokenParsing m => m a -> m [a]
semiSep1 p = sepBy1 p semi
{-# INLINE semiSep1 #-}

-- | Token parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep :: TokenParsing m => m a -> m [a]
commaSep p = sepBy p comma
{-# INLINE commaSep #-}

-- | Token parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep1 :: TokenParsing m => m a -> m [a]
commaSep1 p = sepBy1 p comma
{-# INLINE commaSep1 #-}

-- | Additional functionality that is needed to tokenize input while ignoring whitespace.
class CharParsing m => TokenParsing m where
  -- | Usually, someSpace consists of /one/ or more occurrences of a 'space'.
  -- Some parsers may choose to recognize line comments or block (multi line)
  -- comments as white space as well.
  someSpace :: m ()
  someSpace = skipSome (satisfy isSpace)
  {-# INLINE someSpace #-}

  -- | Called when we enter a nested pair of symbols.
  -- Overloadable to enable disabling layout
  nesting :: m a -> m a
  nesting = id
  {-# INLINE nesting #-}

  -- | The token parser |semi| parses the character \';\' and skips
  -- any trailing white space. Returns the character \';\'. Overloadable to
  -- permit automatic semicolon insertion or Haskell-style layout.
  semi :: m Char
  semi = token (satisfy (';'==) <?> ";")
  {-# INLINE semi #-}

  -- | Tag a region of parsed text with a bit of semantic information.
  -- Most parsers won't use this, but it is indispensible for highlighters.
  highlight :: Highlight -> m a -> m a
  highlight _ a = a
  {-# INLINE highlight #-}

  -- | @token p@ first applies parser @p@ and then the 'whiteSpace'
  -- parser, returning the value of @p@. Every lexical
  -- token (token) is defined using @token@, this way every parse
  -- starts at a point without white space. Parsers that use @token@ are
  -- called /token/ parsers in this document.
  --
  -- The only point where the 'whiteSpace' parser should be
  -- called explicitly is the start of the main parser in order to skip
  -- any leading white space.
  --
  -- Alternatively, one might define 'token' as first parsing 'whiteSpace'
  -- and then parser @p@.  By parsing whiteSpace first, the parser is able
  -- to return before parsing additional whiteSpace, improving laziness.
  --
  -- > mainParser  = sum <$ whiteSpace <*> many (token digit) <* eof
  token :: m a -> m a
  token p = p <* (someSpace <|> pure ())

instance (TokenParsing m, MonadPlus m) => TokenParsing (Lazy.StateT s m) where
  nesting (Lazy.StateT m) = Lazy.StateT $ nesting . m
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (Lazy.StateT m) = Lazy.StateT $ highlight h . m
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m) => TokenParsing (Strict.StateT s m) where
  nesting (Strict.StateT m) = Strict.StateT $ nesting . m
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (Strict.StateT m) = Strict.StateT $ highlight h . m
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m) => TokenParsing (ReaderT e m) where
  nesting (ReaderT m) = ReaderT $ nesting . m
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (ReaderT m) = ReaderT $ highlight h . m
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Strict.WriterT w m) where
  nesting (Strict.WriterT m) = Strict.WriterT $ nesting m
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (Strict.WriterT m) = Strict.WriterT $ highlight h m
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Lazy.WriterT w m) where
  nesting (Lazy.WriterT m) = Lazy.WriterT $ nesting m
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (Lazy.WriterT m) = Lazy.WriterT $ highlight h m
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Lazy.RWST r w s m) where
  nesting (Lazy.RWST m) = Lazy.RWST $ \r s -> nesting (m r s)
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (Lazy.RWST m) = Lazy.RWST $ \r s -> highlight h (m r s)
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Strict.RWST r w s m) where
  nesting (Strict.RWST m) = Strict.RWST $ \r s -> nesting (m r s)
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h (Strict.RWST m) = Strict.RWST $ \r s -> highlight h (m r s)
  {-# INLINE highlight #-}

instance (TokenParsing m, MonadPlus m) => TokenParsing (IdentityT m) where
  nesting = IdentityT . nesting . runIdentityT
  {-# INLINE nesting #-}
  someSpace = lift someSpace
  {-# INLINE someSpace #-}
  semi      = lift semi
  {-# INLINE semi #-}
  highlight h = IdentityT . highlight h . runIdentityT
  {-# INLINE highlight #-}

-- | Used to describe an input style for constructors, values, operators, etc.
data IdentifierStyle m = IdentifierStyle
  { _styleName              :: String
  , _styleStart             :: m Char
  , _styleLetter            :: m Char
  , _styleReserved          :: HashSet String
  , _styleHighlight         :: Highlight
  , _styleReservedHighlight :: Highlight
  }

-- | This lens can be used to update the name for this style of identifier.
--
-- @'styleName' :: Lens' ('IdentifierStyle' m) 'String'@
styleName :: Functor f => (String -> f String) -> IdentifierStyle m -> f (IdentifierStyle m)
styleName f is = (\n -> is { _styleName = n }) <$> f (_styleName is)
{-# INLINE styleName #-}

-- | This lens can be used to update the action used to recognize the first letter in an identifier.
--
-- @'styleStart' :: Lens' ('IdentifierStyle' m) (m 'Char')@
styleStart :: Functor f => (m Char -> f (m Char)) -> IdentifierStyle m -> f (IdentifierStyle m)
styleStart f is = (\n -> is { _styleStart = n }) <$> f (_styleStart is)
{-# INLINE styleStart #-}

-- | This lens can be used to update the action used to recognize subsequent letters in an identifier.
--
-- @'styleLetter' :: Lens' ('IdentifierStyle' m) (m 'Char')@
styleLetter :: Functor f => (m Char -> f (m Char)) -> IdentifierStyle m -> f (IdentifierStyle m)
styleLetter f is = (\n -> is { _styleLetter = n }) <$> f (_styleLetter is)
{-# INLINE styleLetter #-}

-- | This is a traversal of both actions in contained in an 'IdentifierStyle'.
--
-- @'styleChars' :: Traversal ('IdentifierStyle' m) ('IdentifierStyle' n) (m 'Char') (n 'Char')@
styleChars :: Applicative f => (m Char -> f (n Char)) -> IdentifierStyle m -> f (IdentifierStyle n)
styleChars f is = (\n m -> is { _styleStart = n, _styleLetter = m }) <$> f (_styleStart is) <*> f (_styleLetter is)
{-# INLINE styleChars #-}

-- | This is a lens that can be used to modify the reserved identifier set.
--
-- @'styleReserved' :: Lens' ('IdentifierStyle' m) ('HashSet' 'String')@
styleReserved :: Functor f => (HashSet String -> f (HashSet String)) -> IdentifierStyle m -> f (IdentifierStyle m)
styleReserved f is = (\n -> is { _styleReserved = n }) <$> f (_styleReserved is)
{-# INLINE styleReserved #-}

-- | This is a lens that can be used to modify the highlight used for this identifier set.
--
-- @'styleHighlight' :: Lens' ('IdentifierStyle' m) 'Highlight'@
styleHighlight :: Functor f => (Highlight -> f Highlight) -> IdentifierStyle m -> f (IdentifierStyle m)
styleHighlight f is = (\n -> is { _styleHighlight = n }) <$> f (_styleHighlight is)
{-# INLINE styleHighlight #-}

-- | This is a lens that can be used to modify the highlight used for reserved identifiers in this identifier set.
--
-- @'styleReservedHighlight' :: Lens' ('IdentifierStyle' m) 'Highlight'@
styleReservedHighlight :: Functor f => (Highlight -> f Highlight) -> IdentifierStyle m -> f (IdentifierStyle m)
styleReservedHighlight f is = (\n -> is { _styleReservedHighlight = n }) <$> f (_styleReservedHighlight is)
{-# INLINE styleReservedHighlight #-}

-- | This is a traversal that can be used to modify the highlights used for both non-reserved and reserved identifiers in this identifier set.
--
-- @'styleHighlights' :: Traversal' ('IdentifierStyle' m) 'Highlight'@
styleHighlights :: Applicative f => (Highlight -> f Highlight) -> IdentifierStyle m -> f (IdentifierStyle m)
styleHighlights f is = (\n m -> is { _styleHighlight = n, _styleReservedHighlight = m }) <$> f (_styleHighlight is) <*> f (_styleReservedHighlight is)
{-# INLINE styleHighlights #-}

-- | Lift an identifier style into a monad transformer
--
-- Using @over@ from the @lens@ package:
--
-- @'liftIdentifierStyle' = over 'styleChars' 'lift'@
liftIdentifierStyle :: (MonadTrans t, Monad m) => IdentifierStyle m -> IdentifierStyle (t m)
liftIdentifierStyle = runIdentity . styleChars (Identity . lift)
{-# INLINE liftIdentifierStyle #-}

-- | parse a reserved operator or identifier using a given style
reserve :: (TokenParsing m, Monad m) => IdentifierStyle m -> String -> m ()
reserve s name = token $ try $ do
   _ <- highlight (_styleReservedHighlight s) $ string name
   notFollowedBy (_styleLetter s) <?> "end of " ++ show name
{-# INLINE reserve #-}

-- | parse a reserved operator or identifier using a given style given 'Text'.
reserveText :: (TokenParsing m, Monad m) => IdentifierStyle m -> Text -> m ()
reserveText s name = token $ try $ do
   _ <- highlight (_styleReservedHighlight s) $ text name
   notFollowedBy (_styleLetter s) <?> "end of " ++ show name
{-# INLINE reserveText #-}

-- | Parse a non-reserved identifier or symbol
ident :: (TokenParsing m, Monad m, IsString s) => IdentifierStyle m -> m s
ident s = fmap fromString $ token $ try $ do
  name <- highlight (_styleHighlight s)
          ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
  return name
{-# INLINE ident #-}

-- * Utilities

-- | This parser parses a character literal without the surrounding quotation marks.
--
-- This parser does NOT swallow trailing whitespace

characterChar :: TokenParsing m => m Char

charEscape, charLetter :: TokenParsing m => m Char
characterChar = charLetter <|> charEscape <?> "literal character"
{-# INLINE characterChar #-}

charEscape = highlight EscapeCode $ char '\\' *> escapeCode
{-# INLINE charEscape #-}

charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))
{-# INLINE charLetter #-}

-- | This parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
--
-- This parser does NOT swallow trailing whitespace

escapeCode :: forall m. TokenParsing m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl, charNum :: m Char
  charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (char '^' *> (upper <|> char '@'))
  charNum = toEnum <$> num
    where
      num :: m Int
      num = bounded 10 maxchar
        <|> (char 'o' *> bounded 8 maxchar)
        <|> (char 'x' *> bounded 16 maxchar)
      maxchar = fromEnum (maxBound :: Char)

  bounded :: Int -> Int -> m Int
  bounded base bnd = List.foldl' (\x d -> base * x + digitToInt d) 0
                 <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
    where
      thedigits :: [m Char]
      thedigits = map char ['0'..'9'] ++ map oneOf (List.transpose [['A'..'F'],['a'..'f']])

      toomuch :: m a
      toomuch = unexpected "out-of-range numeric escape sequence"

      bounded', bounded'' :: [m Char] -> [Int] -> m [Char]
      bounded' dps@(zero:_) bds = skipSome zero *> ([] <$ notFollowedBy (choice dps) <|> bounded'' dps bds)
                              <|> bounded'' dps bds
      bounded' []           _   = error "bounded called with base 0"
      bounded'' dps []         = [] <$ notFollowedBy (choice dps) <|> toomuch
      bounded'' dps (bd : bds) = let anyd :: m Char
                                     anyd = choice dps

                                     nomore :: m ()
                                     nomore = notFollowedBy anyd <|> toomuch
                                     (low, ex : high) = splitAt bd dps
                                  in ((:) <$> choice low <*> atMost (length bds) anyd) <* nomore
                                     <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                                     <|> if not (null bds)
                                            then (:) <$> choice high <*> atMost (length bds - 1) anyd <* nomore
                                            else empty
      atMost n p | n <= 0    = pure []
                 | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

  charEsc :: m Char
  charEsc = choice $ parseEsc <$> escMap

  parseEsc (c,code) = code <$ char c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

  charAscii :: m Char
  charAscii = choice $ parseAscii <$> asciiMap

  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

-- | This parser parses a natural number (a non-negative whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.
natural' :: TokenParsing m => m Integer
natural' = highlight Number nat <?> "natural"

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  List.foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

-- | This parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.
--
-- Also, unlike the 'integer' parser, this parser does not admit spaces
-- between the sign and the number.

integer' :: TokenParsing m => m Integer
integer' = int <?> "integer"
{-# INLINE integer' #-}

sign :: TokenParsing m => m (Integer -> Integer)
sign = highlight Operator
     $ negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

int :: TokenParsing m => m Integer
int = {-token-} sign <*> highlight Number nat
nat, zeroNumber :: TokenParsing m => m Integer
nat = zeroNumber <|> decimal
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> pure 0) <?> ""

floating :: TokenParsing m => m Scientific
floating = decimal <**> fractExponent
{-# INLINE floating #-}

fractExponent :: forall m. TokenParsing m => m (Integer -> Scientific)
fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1 exponent'
            <|> (\expo n -> fromInteger n * expo) <$> exponent'
 where
  fraction :: m Scientific
  fraction = List.foldl' op 0 <$> (char '.' *> (some digit <?> "fraction"))

  op f d = f + Sci.scientific (fromIntegral (digitToInt d)) (Sci.base10Exponent f - 1)

  exponent' :: m Scientific
  exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"

  power = Sci.scientific 1 . fromInteger


natFloating, zeroNumFloat, decimalFloat :: TokenParsing m => m (Either Integer Scientific)
natFloating
    = char '0' *> zeroNumFloat
  <|> decimalFloat
zeroNumFloat
    = Left <$> (hexadecimal <|> octal)
  <|> decimalFloat
  <|> pure 0 <**> try fractFloat
  <|> pure (Left 0)
decimalFloat = decimal <**> option Left (try fractFloat)

fractFloat :: TokenParsing m => m (Integer -> Either Integer Scientific)
fractFloat = (Right .) <$> fractExponent
{-# INLINE fractFloat #-}

-- | Parses a non-negative whole number in the decimal system. Returns the
-- value of the number.
--
-- This parser does NOT swallow trailing whitespace
decimal :: TokenParsing m => m Integer
decimal = number 10 digit
{-# INLINE decimal #-}

-- | Parses a non-negative whole number in the hexadecimal system. The number
-- should be prefixed with \"x\" or \"X\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
hexadecimal :: TokenParsing m => m Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit
{-# INLINE hexadecimal #-}

-- | Parses a non-negative whole number in the octal system. The number
-- should be prefixed with \"o\" or \"O\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
octal :: TokenParsing m => m Integer
octal = oneOf "oO" *> number 8 octDigit
{-# INLINE octal #-}

-- | This is a parser transformer you can use to disable syntax highlighting
-- over a range of text you are parsing.
newtype Unhighlighted m a = Unhighlighted { runUnhighlighted :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,CharParsing)

instance Parsing m => Parsing (Unhighlighted m) where
  try (Unhighlighted m) = Unhighlighted $ try m
  {-# INLINE try #-}
  Unhighlighted m <?> l = Unhighlighted $ m <?> l
  {-# INLINE (<?>) #-}
  unexpected = Unhighlighted . unexpected
  {-# INLINE unexpected #-}
  eof = Unhighlighted eof
  {-# INLINE eof #-}
  notFollowedBy (Unhighlighted m) = Unhighlighted $ notFollowedBy m
  {-# INLINE notFollowedBy #-}


instance MonadTrans Unhighlighted where
  lift = Unhighlighted
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (Unhighlighted m) where
  get = lift Class.get
  {-# INLINE get #-}
  put = lift . Class.put
  {-# INLINE put #-}

instance MonadReader e m => MonadReader e (Unhighlighted m) where
  ask = lift Class.ask
  {-# INLINE ask #-}
  local f = Unhighlighted . Class.local f . runUnhighlighted
  {-# INLINE local #-}

instance MonadWriter e m => MonadWriter e (Unhighlighted m) where
  tell = lift . Class.tell
  {-# INLINE tell #-}
  listen = Unhighlighted . Class.listen . runUnhighlighted
  {-# INLINE listen #-}
  pass = Unhighlighted . Class.pass . runUnhighlighted
  {-# INLINE pass #-}

instance TokenParsing m => TokenParsing (Unhighlighted m) where
  nesting (Unhighlighted m) = Unhighlighted (nesting m)
  {-# INLINE nesting #-}
  someSpace = Unhighlighted someSpace
  {-# INLINE someSpace #-}
  semi      = Unhighlighted semi
  {-# INLINE semi #-}
  highlight _ m = m
  {-# INLINE highlight #-}

-- | This is a parser transformer you can use to disable the automatic trailing
-- space consumption of a Token parser.
newtype Unspaced m a = Unspaced { runUnspaced :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,CharParsing)

instance Parsing m => Parsing (Unspaced m) where
  try (Unspaced m) = Unspaced $ try m
  {-# INLINE try #-}
  Unspaced m <?> l = Unspaced $ m <?> l
  {-# INLINE (<?>) #-}
  unexpected = Unspaced . unexpected
  {-# INLINE unexpected #-}
  eof = Unspaced eof
  {-# INLINE eof #-}
  notFollowedBy (Unspaced m) = Unspaced $ notFollowedBy m
  {-# INLINE notFollowedBy #-}

instance MonadTrans Unspaced where
  lift = Unspaced
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (Unspaced m) where
  get = lift Class.get
  {-# INLINE get #-}
  put = lift . Class.put
  {-# INLINE put #-}

instance MonadReader e m => MonadReader e (Unspaced m) where
  ask = lift Class.ask
  {-# INLINE ask #-}
  local f = Unspaced . Class.local f . runUnspaced
  {-# INLINE local #-}

instance MonadWriter e m => MonadWriter e (Unspaced m) where
  tell = lift . Class.tell
  {-# INLINE tell #-}
  listen = Unspaced . Class.listen . runUnspaced
  {-# INLINE listen #-}
  pass = Unspaced . Class.pass . runUnspaced
  {-# INLINE pass #-}

instance TokenParsing m => TokenParsing (Unspaced m) where
  nesting (Unspaced m) = Unspaced (nesting m)
  {-# INLINE nesting #-}
  someSpace = empty
  {-# INLINE someSpace #-}
  semi      = Unspaced semi
  {-# INLINE semi #-}
  highlight h (Unspaced m) = Unspaced (highlight h m)
  {-# INLINE highlight #-}

-- | This is a parser transformer you can use to disable the automatic trailing
-- newline (but not whitespace-in-general) consumption of a Token parser.
newtype Unlined m a = Unlined { runUnlined :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,CharParsing)

instance Parsing m => Parsing (Unlined m) where
  try (Unlined m) = Unlined $ try m
  {-# INLINE try #-}
  Unlined m <?> l = Unlined $ m <?> l
  {-# INLINE (<?>) #-}
  unexpected = Unlined . unexpected
  {-# INLINE unexpected #-}
  eof = Unlined eof
  {-# INLINE eof #-}
  notFollowedBy (Unlined m) = Unlined $ notFollowedBy m
  {-# INLINE notFollowedBy #-}

instance MonadTrans Unlined where
  lift = Unlined
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (Unlined m) where
  get = lift Class.get
  {-# INLINE get #-}
  put = lift . Class.put
  {-# INLINE put #-}

instance MonadReader e m => MonadReader e (Unlined m) where
  ask = lift Class.ask
  {-# INLINE ask #-}
  local f = Unlined . Class.local f . runUnlined
  {-# INLINE local #-}

instance MonadWriter e m => MonadWriter e (Unlined m) where
  tell = lift . Class.tell
  {-# INLINE tell #-}
  listen = Unlined . Class.listen . runUnlined
  {-# INLINE listen #-}
  pass = Unlined . Class.pass . runUnlined
  {-# INLINE pass #-}

instance TokenParsing m => TokenParsing (Unlined m) where
  nesting (Unlined m) = Unlined (nesting m)
  {-# INLINE nesting #-}
  someSpace = skipMany (satisfy $ \c -> c /= '\n' && isSpace c)
  {-# INLINE someSpace #-}
  semi      = Unlined semi
  {-# INLINE semi #-}
  highlight h (Unlined m) = Unlined (highlight h m)
  {-# INLINE highlight #-}


instance TokenParsing ReadP.ReadP
