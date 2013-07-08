import "mtl" Control.Monad.State
import Data.Char
import Debug.Trace
import Text.Printf

println msg = trace msg (return ())
minimum :: [Int] -> State Int ()
minimum [] = return ()
minimum xs =
    forM_ xs (\curr -> do
                old_min <- get
                println (printf "old_min: %d curr: %d" old_min curr)
                if (curr < old_min)
                then put curr
                else return ())
-- main = let numbers = [3,2,1] in
--        do print (runState (minimum numbers) (-1))
main = let (n:ns) = [3,2,1] in
       print (runState (Main.minimum ns) n)
