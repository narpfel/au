module Au.Impl.BasicExpression where

import qualified Au.Parser as Au
import Au.Parser hiding (Parser)

type Parser = Au.Parser Char

term :: Integral a => Parser a
term = summand `chainl1` add

add :: Integral a => Parser (a -> a -> a)
add = keywords ["+" .= (+), "-" .= (-)]

summand :: Integral a => Parser a
summand = factor `chainl1` mul

mul :: Integral a => Parser (a -> a -> a)
mul = keywords ["*" .= (*)]

factor :: Integral a => Parser a
factor = atom `chainr1` power

power :: Integral a => Parser (a -> a -> a)
power = keywords ["^" .= (^)]

atom :: Integral a => Parser a
atom = (fromInteger <$> integer) <> parenthesised term
