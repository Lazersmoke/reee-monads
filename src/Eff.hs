module Eff where
data Union (r :: [*->*]) a where
  Inject :: q a -> Union (q ': r) a
  Weaken :: Union (r ': s) a -> Union (x ': r ': s) a

data Eff r a = Pure a | forall x. Eff (Union r x) (TCQ r x a)

run :: Eff '[] a -> a
run (Pure x) = x
run (Eff _ _) = error "User is a magician"

instance Functor (Eff r) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Eff u q) = Eff u (Then q (Singleton (Pure . f)))

instance Applicative (Eff r) where
  pure = Pure
  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Eff u q = Eff u (Then q (Singleton (Pure . f)))
  Eff u q <*> Pure x = Eff u (Then q (Singleton (\f -> Pure . f $ x)))
  Eff uf qf <*> Eff ux qx = Eff uf (Then qf (Singleton (\f -> Eff ux (Then qx (Singleton (Pure . f)))))

instance Monad (Eff r) where
  return = Pure
  Pure x >>= f = Pure (f x)
  Eff u q >>= f = Eff u (Then q f)

data TCQ m a b where
  Singleton :: (a -> m b) -> TCQ m a b
  Then :: TCQ m a b -> TCQ m b c -> TCQ m a c

runTCQ :: TCQ m a b -> a -> m b
runTCQ (Singleton f) = f
runTCQ (Then a b) = (>>= runTCQ b) . runTCQ a

send :: Elem q r => q a -> Eff r a
send q = Eff (Inject q) (Singleton Pure)

data Putstr a where
  PutStr :: String -> Putstr ()

putStrE :: Elem Putstr r => String -> Eff r ()
putStrE = send . PutStr

runPutstr :: Elem IO r => Eff (Putstr ': r) a -> Eff r a
runPutstr (Pure x) = x
runPutstr (Eff u q) = case u of
  Inject (Putstr s) -> send (putStr s) >> runPutstr (Eff u (runTCQ q ()))
  Weaken u' -> Eff u' (Then q (Singleton (\x -> runPutstr
