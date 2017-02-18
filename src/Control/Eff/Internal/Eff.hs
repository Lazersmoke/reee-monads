{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
module Control.Eff.Internal.Eff (Eff(..),run,runM,send) where

import TypeFun.Data.List (Elem)
import Control.Eff.Internal.Union
import Control.Eff.Internal.TCQ

data Eff r a = Pure a | forall x. Eff (Union r x) (TCQ (Eff r) x a)

instance Functor (Eff r) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Eff u q) = Eff u (Then q (Singleton (Pure . f)))

instance Applicative (Eff r) where
  pure = Pure
  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Eff u q = Eff u (Then q (Singleton (Pure . f)))
  Eff u q <*> Pure x = Eff u (Then q (Singleton (\f -> Pure . f $ x)))
  Eff uf qf <*> Eff ux qx = (Eff uf qf) >>= (<$> (Eff ux qx))--Eff uf . Then qf . Singleton $ \f -> Eff ux . Then qx . Singleton . Pure . f

instance Monad (Eff r) where
  return = Pure
  Pure x >>= f = f x
  Eff u q >>= f = Eff u (Then q (Singleton f))

run :: Eff '[] a -> a
run (Pure x) = x
run (Eff _ _) = error "User is a magician"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Eff u q) = extract u >>= runM . runTCQ q

send :: Member q r => q a -> Eff r a
send qa = Eff (inject qa) (Singleton Pure)
