import System.Random
import Control.Monad.State
import Control.Monad.Writer

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
  f <- mf
  x <- m
  return (f x)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False
