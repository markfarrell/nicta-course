module Course.Applicative where

import Course.Functor

class (Functor f) <= Applicative f where
  pure :: forall a. a -> f a
  ap :: forall a b. f (a -> b) -> f a -> f b

(<*>) = ap

(<$>) :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
(<$>) f fa = (pure f) <*> fa

lift2 :: forall f a b c. (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
lift2 f fa fb = f <$> fa <*> fb

lift3 :: forall f a b c d. (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 f fa fb fc = f <$> fa <*> fb <*> fc

lift4 :: forall f a b c d e. (Applicative f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4 f fa fb fc fd = f <$> fa <*> fb <*> fc <*> fd

(<*) :: forall f a b. (Applicative f) => f a -> f b -> f a
(<*) = lift2 const where
  const :: forall a b. a -> b -> a
  const a _ = a

(*>) :: forall f a b. (Applicative f) => f a -> f b -> f b
(*>) = lift2 coconst where
  coconst :: forall a b. a -> b -> b
  coconst _ b = b
