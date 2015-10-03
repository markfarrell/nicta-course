module Course.Functor where

import Course.Id
import Course.Optional
import Course.Validation

-- Functor Laws
-- ------------
-- Law of identity: forall f. (Functor f) => id <$> f ≅ f
-- Law of composition: forall f g f'. (Functor f') => (f <<< g <$> f') ≅ (f <$> (g <$> f'))

class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b

instance functorId :: Functor Id where
  (<$>) = mapId

instance functorOptional :: Functor Optional where
  (<$>) = mapOptional

instance functorValidation :: Functor Validation where
  (<$>) = mapValidation
