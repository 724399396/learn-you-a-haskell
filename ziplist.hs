import Control.Applicative

--instance Applicative ZipList where
--  pure x = ZipList (repeat x)
--  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

--sequenceA :: (Applicative f) => [f a] -> f [a]
--sequenceA [] = pure []
--sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
