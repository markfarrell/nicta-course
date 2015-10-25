module Course.State where

import Data.Tuple

import Course.Unit

data State s a = State s a

mapState :: forall a b s. (a -> b) -> State s a -> State s b
mapState f (State s a) = State s (f a)

applyState :: forall a b s. State s (a -> b) -> State s a -> State s b
applyState (State s f) (State _ a) = State s (f a)

pureState :: forall a s. s -> a -> State s a
pureState s a = State s a

bindState :: forall a b s. (a -> State s b) -> State s a -> State s b
bindState f (State _ a) = f a

getState :: forall s. State s s -> s
getState (State s _) = s

putState :: forall s. s -> State s Unit
putState s = (State s unit)

runState :: forall a s. State s a -> s -> Tuple a s
runState (State _ a) s = (Tuple a s)

evalState :: forall s a. State s a -> s -> a
evalState state s' = fst (runState state s')

execState :: forall s a. State s a -> s -> s
execState state s' = snd (runState state s')
