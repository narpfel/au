module JsonDemo.Main where

import Au.Impl.Json

main :: IO ()
main = do
  print (read "{\"foo\": [1.4, 20.1e3, null, true]}" :: Json)
  print (read "[[]   ]" :: Json)
