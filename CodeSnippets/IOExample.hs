import Data.Char
main :: IO ()
main = do
  writeFile "test.txt" "a,b,c,d,e"
  x <- readFile "test.txt"
  let upCased = map toUpper x
  y <- return upCased 
  print y
