import Data.List

solveRPN :: (Read a, Num a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x*y):ys
          foldingFunction (x:y:ys) "+" = (x+y):ys
          foldingFunction (x:y:ys) "-" = (y-x):ys
          foldingFunction ys numStr = read numStr:ys
    