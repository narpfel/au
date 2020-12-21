module BasicExpression.Main (main) where

import Au.Impl.BasicExpression
import Au.Parser

main :: IO ()
main = do
  print . parse (term @Integer) $ "3^2^2^2"
