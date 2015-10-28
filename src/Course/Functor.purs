module Course.Functor where

class Functor f where
  fmap :: forall a b. (a -> b) -> f a -> f b
