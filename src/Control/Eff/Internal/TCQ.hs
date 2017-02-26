{-# LANGUAGE GADTs #-}
module Control.Eff.Internal.TCQ (TCQ(..),runTCQM,ViewL(..),viewl) where

import Data.Profunctor
import Control.Arrow
import qualified Control.Category as C

data TCQ m a b where
  Singleton :: (a -> m b) -> TCQ m a b
  Then :: TCQ m a b -> TCQ m b c -> TCQ m a c

instance Monad m => C.Category (TCQ m) where
  id = Singleton return
  (.) = flip Then

instance Monad m => Arrow (TCQ m) where
  arr f = Singleton (return . f)
  first tcq = Singleton (\ ~(a,c) -> runTCQM tcq a >>= \b -> return (b,c))

instance Monad m => Profunctor (TCQ m) where
  lmap f tcq = Then (Singleton (return . f)) tcq
  rmap f tcq = Then tcq (Singleton (return . f))

instance Monad m => Strong (TCQ m) where
  first' = first

--instance Monad m => Choice (TCQ m) where
  --left' tcq = Singleton $ \eAC -> case eAC of
    --Left a -> Then tcq (return . Left)
    --Right c -> return (Right c)

instance Monad m => Functor (TCQ m a) where
  fmap f tcq = Then tcq (Singleton (return . f))

{-# INLINE runTCQM #-}
runTCQM :: Monad m => TCQ m a b -> a -> m b
runTCQM (Singleton f) = f
runTCQM (Then a b) = (>>= runTCQM b) . runTCQM a

data ViewL m a b where
  FirstL :: (a -> m b) -> ViewL m a b
  ConsL :: (a -> m x) -> TCQ m x b -> ViewL m a b

viewl :: TCQ m a b -> ViewL m a b
viewl (Singleton f) = FirstL f
viewl (Then f tcq) = go f tcq
  where
    go :: TCQ m a x -> TCQ m x b -> ViewL m a b
    go (Singleton g) cont = ConsL g cont
    go (Then a b) cont = go a (Then b cont)
  
