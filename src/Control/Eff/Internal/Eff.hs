{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
module Control.Eff.Internal.Eff (Eff(..),run,runM,runTCQ,send) where

import Control.Eff.Internal.Union
import Control.Eff.Internal.TCQ

data Eff r a = Pure a | forall x. Eff (Union r x) (TCQ (Eff r) x a)

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f (Pure a) = Pure (f a)
  fmap f (Eff u q) = Eff u (Then q (Singleton (Pure . f)))

instance Applicative (Eff r) where
  {-# INLINE pure #-}
  pure = Pure
  {-# INLINE (<*>) #-}
  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Eff u q = Eff u (Then q (Singleton (Pure . f)))
  Eff u q <*> Pure x = Eff u (Then q (Singleton (Pure . ($x))))
  Eff u q <*> ex = Eff u (Then q (Singleton (<$> ex)))

instance Monad (Eff r) where
  {-# INLINE return #-}
  return = Pure
  {-# INLINE (>>=) #-}
  Pure x >>= f = f x
  Eff u q >>= f = Eff u (Then q (Singleton f))

{-# INLINE runTCQ #-}
runTCQ :: TCQ (Eff r) a b -> a -> Eff r b
runTCQ tcq x = case viewl tcq of
  FirstL k -> k x
  ConsL k t -> case k x of
    Pure n -> runTCQ t n
    Eff u q -> Eff u (Then q t)

run :: Eff '[] a -> a
run (Pure x) = x
run (Eff _ _) = error "User is a magician"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Eff u q) = extract u >>= runM . runTCQ q

{-# INLINE send #-}
send :: Member q r => q a -> Eff r a
send qa = Eff (inject qa) (Singleton Pure)
