import Data.Char
main :: IO ()
main = do
  writeFile "test.txt" "a,b,c,d,e"
  x <- readFile "test.txt"
  let up_cased = map toUpper x
  y <- return up_cased 
  print y
