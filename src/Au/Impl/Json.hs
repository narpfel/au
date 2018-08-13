module Au.Impl.Json where

import qualified Data.Map.Strict as Map

import qualified Au.Impl.JsonToken as JsonToken (Token(..), jsonToken)
import Au.Parser

type JsonParser = Parser JsonToken.Token

data Json
  = Array [Json]
  | Object (Map.Map String Json)
  | Str String
  | Number Double
  | True'
  | False'
  | Null
  deriving (Eq, Show)

instance Read Json where
  readsPrec _ = readSFromParser JsonToken.jsonToken json

json :: JsonParser Json
json = array <> object <> literal

array :: JsonParser Json
array =
  between
    (char JsonToken.OpenBracket)
    (Array <$> optional elements)
    (char JsonToken.CloseBracket)

elements :: JsonParser [Json]
elements = separatedBy (char JsonToken.Comma) json

object :: JsonParser Json
object =
  Object <$>
  between
    (char JsonToken.OpenBrace)
    (Map.fromList <$> members)
    (char JsonToken.CloseBrace)

members :: JsonParser [(String, Json)]
members = optional pairs

pairs :: JsonParser [(String, Json)]
pairs = separatedBy (char JsonToken.Comma) pair

pair :: JsonParser (String, Json)
pair = do
  key <- anything >>= \case
      (JsonToken.Str s) -> pure s
      _ -> mempty
  _ <- char JsonToken.Colon
  value <- json
  pure (key, value)

literal :: JsonParser Json
literal = anything >>= \case
  JsonToken.Str s -> pure (Str s)
  JsonToken.Number x -> pure (Number x)
  JsonToken.True' -> pure True'
  JsonToken.False' -> pure False'
  JsonToken.Null -> pure Null
  _ -> mempty
