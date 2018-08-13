module Au.Impl.JsonToken where

import Control.Applicative (some, many)
import Data.Char (isControl)

import Au.Parser

data Token
  = OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | Comma
  | Colon
  | True'
  | False'
  | Null
  | Str String
  | Number Double
  deriving (Show, Eq)

jsonToken :: Tokenizer Token
jsonToken = filterSpace $ jsonKeyword <> string <> number

jsonKeyword :: Tokenizer Token
jsonKeyword =
  keywords
    [ ("{", OpenBrace)
    , ("}", CloseBrace)
    , ("[", OpenBracket)
    , ("]", CloseBracket)
    , (",", Comma)
    , (":", Colon)
    , ("true", True')
    , ("false", False')
    , ("null", Null)
    ]

string :: Tokenizer Token
string = Str <$> (quote *> stringContent <* quote)

-- TODO: Accept unescaped DEL characters (0x7F).
stringContent :: Tokenizer String
stringContent = many $
  escaped escapable
  <> unicodeEscape
  <> matchesAll [not . isControl, (/= '\\'), (/= '"')]

escaped :: Tokenizer Char -> Tokenizer Char
escaped = (char '\\' >>)

escapable :: Tokenizer Char
escapable = choice . map char $ "\"\\/bfnrt"

unicodeEscape :: Tokenizer Char
unicodeEscape = escaped $ char 'u' >> ((toEnum . read . ("0x" ++)) <$> exactly 4 hexdigit)

quote :: Tokenizer Char
quote = char '"'

number :: Tokenizer Token
number = Number . read <$> number'

number' :: Tokenizer String
number' = optionalMinus <++> numberStart <++> decimalPart <++> exponentPart

numberStart :: Tokenizer String
numberStart = (pure <$> zero) <> (nonzero <:> many digit)

decimalPart :: Tokenizer String
decimalPart = optional $ word "." <++> some digit

exponentPart :: Tokenizer String
exponentPart = optional $
  (word "e" <> word "E") <++>
  (word "-" <> (word "+" >> pure "") <> nothing) <++>
  many digit

optionalMinus :: Tokenizer String
optionalMinus = optional $ word "-"
