module Course.Id where
    
import Prelude
import Control.Monad.Eff.Console (log)

data Id a = Id a

instance showId :: (Show a) => Show (Id a) where
  show (Id a) = show a
  
runId :: forall a. Id a -> a
runId (Id a) = a

mapId :: forall a b. (a -> b) -> Id a -> Id b
mapId f (Id a) = Id (f a)

bindId :: forall a b. (a -> Id b) -> Id a -> Id b
bindId f (Id a) = f a