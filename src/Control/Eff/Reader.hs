{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Eff.Reader 
  (Reader
  ,ask
  ,reader
  ,runReaderStrictly
  ,runReaderLazily
  ) where

import Control.Eff

data Reader e a where
  Ask :: Reader e e

ask :: Member (Reader e) r => Eff r e
ask = send Ask

reader :: Member (Reader e) r => (e -> a) -> Eff r a
reader = (<$> ask)

--local :: Member (Reader e) r => (e -> e) -> Eff r a -> Eff r a
--local f m = do
  --e <- ask
  --let e' = f e
  --let 
    --loop m' = 
     --case m' of
      --Pure x -> return x
      --Eff u q -> case project u of
        --Just Ask -> ((\g h a -> h $ runTCQ g a) q loop) e'
        --Nothing -> Eff u (Singleton (loop . runTCQ q))
  --loop m

runReaderStrictly :: e -> Eff (Reader e ': r) a -> Eff r a
runReaderStrictly = runReader True

runReaderLazily :: e -> Eff (Reader e ': r) a -> Eff r a
runReaderLazily = runReader False

runReader :: Bool -> e -> Eff (Reader e ': r) a -> Eff r a
runReader strict e (Pure x) = Pure . (if strict then seq e else id) $ x
runReader strict e (Eff u q) = (if strict then seq e else id) $ case u of
  Inject Ask -> runReader strict e (runTCQ q e)
  Weaken u' -> Eff u' (Singleton (runReader strict e . runTCQ q))
