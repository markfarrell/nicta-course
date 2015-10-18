module Course.Optional where

import Prelude

data Optional a = Empty | Full a

instance showOptional :: (Show a) => Show (Optional a) where
  show Empty = ""
  show (Full a) = show a

mapOptional :: forall a b. (a -> b) -> Optional a -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

(<+>) :: forall a. Optional a -> Optional a -> Optional a
(<+>) Empty o = o
(<+>) k _ = k

