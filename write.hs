import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

--logNumber :: Int -> Writer [String] Int
--logNumber x = Writer (x, ["Got number:" ++ show x])

--multWithLog :: Writer [String] Int
--multWithLog = do
--  a <- logNumber 3
--  b <- logNumber 5
--  return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
     tell ["Finished with " ++ show a]
     return a
  | otherwise = do
     tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
     gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
     tell ["Finished with " ++ show a]
     return a
  | otherwise = do
     result <- gcdReverse b (a `mod` b)
     tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
     return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x-1)
  tell (toDiffList [show x])
