module Course.Validation where

import Course.Functor
import Course.Applicative

data Validation a = Error Err | Value a

type Err = String

instance functorValidation :: Functor Validation where
  fmap _ (Error s) = Error s
  fmap f (Value a) = Value (f a)

foreign import concatString :: String -> String -> String

instance applicativeValidation :: Applicative Validation where
  pure = Value
  ap (Value f) (Value a) = Value (f a)
  ap (Error e) (Value a) = Error e
  ap (Value f) (Error e) = Error e
  ap (Error e1) (Error e2) = Error (concatString e1 e2)

bindValidation :: forall a b. (a -> Validation b) -> Validation a -> Validation b
bindValidation _ (Error s) = Error s
bindValidation f (Value a) = f a
