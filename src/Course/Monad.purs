module Course.Monad where

import Course.Id
import Course.Applicative
import Course.Optional
import Course.Validation

class (Applicative f) <= Monad f where
  (=<<) :: forall a b. (a -> f b) -> f a -> f b

instance monadId :: Monad Id where
  (=<<) f (Id a) = f a

instance monadOptional :: Monad Optional where
  (=<<) f Empty = Empty
  (=<<) f (Full a) = f a

instance monadValidation :: Monad Validation where
  (=<<) f (Error e) = (Error e)
  (=<<) f (Value a) = f a

-- (<*>) :: forall a b f. (Monad f) => f (a -> b) -> f a -> f b
-- join :: forall a f. (Monad f) => f (f a) -> f a
-- (>>=) :: forall a f. (Monad f) => f a -> (a -> f b) -> f b
