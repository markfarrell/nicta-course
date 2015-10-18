module Course.Monad where

import Course.Id
import Course.Optional
import Course.Validation
import Course.Functor
import Course.Applicative

class (Applicative f) <= Monad f where
  bind :: forall a b. (a -> f b) -> f a -> f b

instance monadId :: Monad Id where
  bind f (Id a) = f a

instance monadOptional :: Monad Optional where
  bind f Empty = Empty
  bind f (Full a) = f a

instance monadValidation :: Monad Validation where
  bind f (Error e) = (Error e)
  bind f (Value a) = f a

(=<<) = bind

return = pure

apM :: forall a b f. (Monad f) => f (a -> b) -> f a -> f b
apM f a = bind (\f' -> fmap (\a' -> f' a') a) f

(>>=) :: forall a b f. (Monad f) => f a -> (a -> f b) -> f b
(>>=) a f = bind f a 

join :: forall a f. (Monad f) => f (f a) -> f a
join = bind id where
  id :: forall a. a -> a
  id a = a

(>=>) :: forall a b c f. (Monad f) => (a -> f b) -> (b -> f c) -> (a -> f c)
(>=>) f g = (\a -> bind (\b -> g b) (f a))

(<=<) :: forall a b c f. (Monad f) => (b -> f c) -> (a -> f b) -> (a -> f c)
(<=<) g f = f >=> g
