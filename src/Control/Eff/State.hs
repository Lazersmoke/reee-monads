{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Eff.State
  (State
  ,get
  ,put
  ,modify
  ,runState,runState'
  ,execState,execState'
  ,evalState,evalState'
  ) where

import Control.Eff

data State s a where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) r => Eff r s
get = send Get

put :: Member (State s) r => s -> Eff r ()
put = send . Put

modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = put . f =<< get

execState', execState :: s -> Eff (State s ': r) a -> Eff r s
execState' s e = snd <$> runState' s e
execState s e = snd <$> runState s e

evalState', evalState :: s -> Eff (State s ': r) a -> Eff r a
evalState' s e = fst <$> runState' s e
evalState s e = fst <$> runState s e

runState', runState :: s -> Eff (State s ': r) a -> Eff r (a,s)
runState' = runStateCore True
runState = runStateCore False

runStateCore :: Bool -> s -> Eff (State s ': r) a -> Eff r (a,s)
runStateCore strict s (Pure x) = Pure ((if strict then seq s else id) x,s)
runStateCore strict s (Eff u q) = (if strict then seq s else id) $ case u of
  Inject Get -> runStateCore strict s (runTCQ q s)
  Inject (Put s') -> runStateCore strict s' (runTCQ q ())
  Weaken u' -> Eff u' (Singleton (runStateCore strict s . runTCQ q))
