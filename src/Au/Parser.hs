module Au.Parser where

import Control.Applicative (Alternative, (<|>), empty, some, many)
import Control.Monad (ap, liftM2)
import Data.Char (isSpace, isAlpha, isAlphaNum, isSymbol, isPunctuation)
import Data.Composition ((.:))
import Data.Foldable (asum)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)

newtype Parser c a = Parser
  { run :: [c] -> [(a, [c])]
  } deriving Functor

type Tokenizer = Parser Char

instance Applicative (Parser c) where
  pure v = Parser {run = \cs -> [(v, cs)]}
  (<*>) = ap

instance Semigroup (Parser c a) where
  a <> b = Parser {run = \cs -> run a cs <> run b cs}

instance Monoid (Parser c a) where
  mempty = Parser {run = const []}

instance Monad (Parser c) where
  parser >>= f =
    Parser
    { run =
        \cs ->
          [ (b, cs'')
          | (a, cs') <- run parser cs
          , (b, cs'') <- run (f a) cs'
          ]
    }

instance Alternative (Parser c) where
  empty = mempty
  (<|>) = (<>)

parse :: Parser c a -> [c] -> Maybe a
parse = listToMaybe .: parseAll

parseAll :: Parser c a -> [c] -> [a]
parseAll = map fst . filter (null . snd) .: run

infixr 5 <++>
(<++>) :: Parser c [a] -> Parser c [a] -> Parser c [a]
(<++>) = liftM2 (++)

infixr 5 <:>
(<:>) :: Parser c a -> Parser c [a] -> Parser c [a]
(<:>) = liftM2 (:)

space :: Parser Char Char
space = matches isSpace

newline :: Parser Char Char
newline = char '\n'

matches :: (c -> Bool) -> Parser c c
matches predicate =
  Parser $ \case
    (c:cs) | predicate c -> [(c, cs)]
    _ -> []

matchesAll :: [c -> Bool] -> Parser c c
matchesAll ps = matches $ and . sequence ps

choice :: [Parser c a] -> Parser c a
choice = asum

char :: Eq c => c -> Parser c c
char c = matches (== c)

word :: Eq c => [c] -> Parser c [c]
word [] = pure []
word (c:cs) = char c <:> word cs

no :: Eq c => c -> Parser c c
no c = matches (/= c)

neither :: Eq c => [c] -> Parser c c
neither = matchesAll . map (/=)

exactly :: Integer -> Parser c a -> Parser c [a]
exactly 0 _ = pure []
exactly n p = p <:> exactly (n - 1) p

between :: Parser c b -> Parser c a -> Parser c d -> Parser c a
between begin parser end = begin *> parser <* end

between' :: Eq c => [c] -> [c] -> Parser c a -> Parser c a
between' begin end parser = word begin *> parser <* word end

filterSpace :: Parser Char a -> Parser Char a
filterSpace parser = many space *> parser <* many space

wordToken :: Eq c => [c] -> a -> Parser c a
wordToken c t = word c >> pure t

keywords :: Eq c => [([c], a)] -> Parser c a
keywords = choice . (map . uncurry) wordToken

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (c -> d) -> (a, c) -> (a, d)
mapSnd f (a, c) = (a, f c)

optional :: Monoid a => Parser c a -> Parser c a
optional = optionalOr mempty

optional' :: Parser c a -> Parser c (Maybe a)
optional' p = (Just <$> p) <> pure Nothing

optionalOr :: a -> Parser c a -> Parser c a
optionalOr a p = fromMaybe a <$> optional' p

optionalWord :: Eq c => [c] -> Parser c [c]
optionalWord s = optional $ word s

minus :: Parser Char Char
minus = char '-'

zero :: Parser Char Char
zero = char '0'

nonzero :: Parser Char Char
nonzero = choice . map char $ "123456789"

digit :: Parser Char Char
digit = zero <> nonzero

hexdigit :: Parser Char Char
hexdigit = digit <> (choice . map char $ "abcdefABCDEF")

integer :: (Read a, Integral a) => Parser Char a
integer = read <$> optionalWord "-" <++> some digit

alphabetical :: Parser Char String
alphabetical = some $ matches isAlpha

alphanum :: Parser Char String
alphanum = some $ matches isAlphaNum

operator :: Parser Char String
operator = some . choice . map matches $ [isSymbol, isPunctuation]

parenthesised :: Parser Char a -> Parser Char a
parenthesised = between' "(" ")"

braced :: Parser Char a -> Parser Char a
braced = between' "{" "}"

bracketed :: Parser Char a -> Parser Char a
bracketed = between' "[" "]"

separatedBy :: Parser c a -> Parser c b -> Parser c [b]
separatedBy sep parser = parser <:> optional (sep >> separatedBy sep parser)

separatedBy' :: Parser c a -> Parser c b -> Parser c [b]
separatedBy' sep parser = separatedBy sep parser <* optional' sep

anything :: Parser c c
anything = matches $ const True

nothing :: Monoid a => Parser c a
nothing = pure mempty

perLine :: Parser Char a -> Parser Char [a]
perLine = separatedBy' newline

peek :: Parser c c
peek =
  Parser
  { run =
      \case
          [] -> []
          c:cs -> [(c, c:cs)]
  }

readSFromParser :: Show a => Tokenizer a -> Parser a b -> ReadS b
readSFromParser tokenizer parser =
  map (mapSnd $ concatMap show) . run parser . fromJust . parse (some tokenizer)
