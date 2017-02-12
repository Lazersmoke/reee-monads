{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Eff.Writer
  (Writer
  ,tell
  ,runWriter,runWriter'
  ,evalWriter,evalWriter'
  ,execWriter,execWriter'
  ) where

import Control.Eff

data Writer w a where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) r => w -> Eff r ()
tell = send . Tell

execWriter', execWriter :: Monoid w => Eff (Writer w ': r) a -> Eff r w
execWriter' e = snd <$> runWriter' e
execWriter e = snd <$> runWriter e

evalWriter', evalWriter :: Monoid w => Eff (Writer w ': r) a -> Eff r a
evalWriter' e = fst <$> runWriter' e
evalWriter e = fst <$> runWriter e

runWriter', runWriter :: Monoid w => Eff (Writer w ': r) a -> Eff r (a,w)
runWriter' = runWriterCore True mempty
runWriter = runWriterCore False mempty

runWriterCore :: Monoid m => Bool -> m -> Eff (Writer m ': r) a -> Eff r (a,m)
runWriterCore strict w (Pure x) = (if strict then seq w else id) $ Pure (x,w)
runWriterCore strict w (Eff u q) = (if strict then seq w else id) $ case u of
  Inject (Tell w') -> runWriterCore strict (mappend w w') (runTCQ q ())
  Weaken u' -> Eff u' (Singleton (runWriterCore strict w . runTCQ q))
