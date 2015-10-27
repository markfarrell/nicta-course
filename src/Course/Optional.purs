module Course.Optional where

import Course.Functor
import Course.Applicative
import Course.Monad

data Optional a = Empty | Full a

instance functorOptional :: Functor Optional where
  fmap _ Empty = Empty
  fmap f (Full a) = Full (f a)

instance applicativeOptional :: Applicative Optional where
  pure a = Full a
  ap (Full f) (Full a) = Full (f a)
  ap _ _ = Empty

instance monadOptional :: Monad Optional where
  bind f Empty = Empty
  bind f (Full a) = f a


