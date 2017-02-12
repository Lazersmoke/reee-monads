{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Eff.State
  (State
  ,get
  ,put
  ,modify
  ,runStateStrictly
  ,runStateLazily
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

runStateStrictly :: s -> Eff (State s ': r) a -> Eff r a
runStateStrictly = runState True

runStateLazily :: s -> Eff (State s ': r) a -> Eff r a
runStateLazily = runState False

runState :: Bool -> s -> Eff (State s ': r) a -> Eff r a
runState strict s (Pure x) = Pure $ (if strict then seq s else id) x
runState strict s (Eff u q) = (if strict then seq s else id) $ case u of
  Inject Get -> runState strict s (runTCQ q s)
  Inject (Put s') -> runState strict s' (runTCQ q ())
  Weaken u' -> Eff u' (Singleton (runState strict s . runTCQ q))
