{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char
main :: IO ()
main = do
  writeFile "test.txt" "a,b,c,d,e" :: IO ()
  x :: String <- readFile "test.txt" :: IO String
  let upCased :: String = map toUpper x
  y :: String <- return upCased :: IO String
  print y :: IO ()
