module Course.Id where

import Course.Functor
import Course.Applicative
import Course.Monad

newtype Id a = Id a

instance functorId :: Functor Id where
  fmap f (Id a) = Id (f a)

instance applicativeId :: Applicative Id where
  pure a = Id a
  ap (Id f) (Id a) = Id (f a)

instance monadId :: Monad Id where
  bind f (Id a) = f a
