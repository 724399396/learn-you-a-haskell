import Data.Monoid

newtype First' a = First' { getFirst' :: Maybe a }
  deriving (Eq, Ord, Read, Show)

instance Monoid (First' a) where
  mempty = First' Nothing
  First' (Just x) `mappend` _ = First' (Just x)
  First' Nothing `mappend` x = x
