{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Eff.Trace where

import Control.Eff

data Trace a where
  Trace :: String -> Trace ()

trace :: Member Trace r => String -> Eff r ()
trace = send . Trace

runTrace :: Member IO r => Eff (Trace ': r) a -> Eff r a
runTrace (Pure x) = Pure x
runTrace (Eff u q) = case u of
  Inject (Trace s) -> do
    send $ putStrLn s
    runTrace (runTCQ q ())
  Weaken u' -> Eff u' (Singleton (runTrace . runTCQ q))
