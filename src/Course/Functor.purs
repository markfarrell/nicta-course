module Course.Functor where

import Course.Id
import Course.Optional
import Course.Validation

class Functor f where
  fmap :: forall a b. (a -> b) -> f a -> f b

instance functorId :: Functor Id where
  fmap = mapId

instance functorOptional :: Functor Optional where
  fmap = mapOptional

instance functorValidation :: Functor Validation where
  fmap = mapValidation
