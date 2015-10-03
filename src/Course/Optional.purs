module Course.Optional where

import Prelude

data Optional a = Empty | Full a

instance showOptional :: (Show a) => Show (Optional a) where
  show Empty = ""
  show (Full a) = "test"

mapOptional :: forall a b. (a -> b) -> Optional a -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

bindOptional :: forall a b. (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty = Empty
bindOptional f (Full a) = f a

(<?) :: forall a. Optional a -> a -> a
(<?) Empty d = d
(<?) (Full a) _ = a

(<+>) :: forall a. Optional a -> Optional a -> Optional a
(<+>) Empty o = o
(<+>) k _ = k

applyOptional :: forall a b. Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional (\a' -> f' a') a) f

twiceOptional :: forall a b c. (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional <<< (mapOptional f)

contains :: forall a. (Eq a) => a -> Optional a -> Boolean
contains _ Empty = false
contains a (Full z) = (a == z)
