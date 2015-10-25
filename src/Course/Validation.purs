module Course.Validation where

import Prelude

data Validation a = Error Err | Value a

type Err = String

mapValidation :: forall a b. (a -> b) -> Validation a -> Validation b
mapValidation _ (Error s) = Error s
mapValidation f (Value a) = Value (f a)

bindValidation :: forall a b. (a -> Validation b) -> Validation a -> Validation b
bindValidation _ (Error s) = Error s
bindValidation f (Value a) = f a

appendErr :: Err -> Err -> Err
appendErr e1 e2 = e1 ++ e2
