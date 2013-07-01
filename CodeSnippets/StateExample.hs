import "mtl" Control.Monad.State
import Data.Char
import Debug.Trace
import Text.Printf

println msg = trace msg (return ())
minimum_bad :: [Int] -> ((), Int)
minimum_bad [] = error "Empty List."
minimum_bad xs =
    runState (mapM_ compare xs :: State Int ()) (-1)
    where
      compare :: Int -> State Int ()
      compare curr = do
              old_min <- get
              println (printf "old_min: %d curr: %d" old_min curr)
              if (curr < old_min)
              then put curr
              else return ()

minimum_good :: [Int] -> ((), Int)
minimum_good [] = error "Empty List."
minimum_good (x:xs) =
    runState (mapM_ compare xs :: State Int ()) x
    where
      compare :: Int -> State Int ()
      compare curr = do
              old_min <- get
              println ("old_min: " ++ (show old_min))
              if (curr < old_min)
              then put curr
              else return ()
