{-# LANGUAGE GADTs #-}
module Control.Eff.Reader 
  (Reader
  ,ask
  ,runReaderStrictly
  ,runReaderLazily
  ) where

import Control.Eff

data Reader e a where
  Ask :: Reader e
  Local :: (e -> e) -> Reader e -> Reader e

ask :: Elem (Reader e) r => Eff r e
ask = send . Ask

local :: Elem (Reader e) r => (e -> e) -> Eff r e -> Eff r e
local = send . Local

reader :: Elem (Reader e) r => (e -> a) -> Eff r a
reader = (<$> ask)

runReaderStrictly :: e -> Eff (Reader e ': r) a -> Eff r a
runReaderStrictly = runReader True

runReaderLazily :: e -> Eff (Reader e ': r) a -> Eff r a
runReaderLazily = runReader False

runReader :: Bool -> e -> Eff (Reader e ': r) a -> Eff r a
runReader strict e (Pure x) = (if strict then seq e else id) x
runReader strict e (Eff u q) = (if strict then seq e else id) case u of
  Inject Ask -> runReader strict e $ Eff u (runTCQ q e)
  Inject (Local f m) -> runReader strict (f e) m
  Weaken u' -> Eff u' $ Then q (Singleton (runReader strict e . runTCQ q))
