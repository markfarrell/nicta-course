module Course.State where

import Data.Tuple

import Course.Unit

data State s a = State (s -> Tuple a s)

get :: forall s. State s s
get = State (\s -> Tuple s s)

put :: forall s. s -> State s Unit
put s = (State (\_ -> Tuple unit s))

runState :: forall a s. State s a -> s -> Tuple a s
runState (State f) s = f s

evalState :: forall s a. State s a -> s -> a
evalState state s' = fst (runState state s')

execState :: forall s a. State s a -> s -> s
execState state s' = snd (runState state s')
