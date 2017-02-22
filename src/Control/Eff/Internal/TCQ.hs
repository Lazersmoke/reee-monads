{-# LANGUAGE GADTs #-}
module Control.Eff.Internal.TCQ (TCQ(..), runTCQ) where

data TCQ m a b where
  Singleton :: (a -> m b) -> TCQ m a b
  Then :: TCQ m a b -> TCQ m b c -> TCQ m a c

{-# INLINE runTCQ #-}
runTCQ :: Monad m => TCQ m a b -> a -> m b
runTCQ (Singleton f) = f
runTCQ (Then a b) = (>>= runTCQ b) . runTCQ a


