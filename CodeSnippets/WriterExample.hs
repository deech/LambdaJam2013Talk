import "mtl" Control.Monad.Writer
import Data.Char
import Debug.Trace

validate :: String -> Writer [String] ()
validate input =
    let hasNumbers = (>= 2) . length . filter isDigit
        hasUppers  = (>= 1) . length . filter isUpper
        noSpaces   = null . filter (== ' ')
        check f msg = if (not (f input))
                      then tell [msg]
                      else return ()
    in do check hasNumbers "Needs 2+ numbers"
          check hasUppers  "Needs 1+ capitals"
          check noSpaces   "Has spaces"

main = do
  let ((),errs) = runWriter (validate "abcde1")
      valid = null errs
  if (not valid) then print errs else print "Valid!"
