{-# LANGUAGE GADTs #-}
module Control.Eff.Trace where

import Control.Eff

data Trace a where
  Trace :: String -> Trace ()

trace :: Elem Trace r => String -> Eff r ()
trace = send . Trace

runTrace :: Elem IO r => Eff (Putstr ': r) a -> Eff r a
runTrace (Pure x) = x
runTrace (Eff u q) = case u of
  Inject (Trace s) -> do
    send $ putStrLn s
    runPutstr $ Eff u (runTCQ q ())
  Weaken u' -> Eff u' $ Then q (Singleton (runTrace . runTCQ q))
