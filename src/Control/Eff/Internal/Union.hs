{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Eff.Internal.Union (Union(..),Member,project,inject,extract) where
import Data.Proxy
import TypeFun.Data.List hiding (Union,IndexOf)
import TypeFun.Data.Peano

data Union (r :: [* -> *]) a where
  Inject :: q a -> Union (q ': r) a
  Weaken :: Union (r ': s) a -> Union (x ': r ': s) a

class MemberAt q r (n :: N) where
  injectAt :: Proxy n -> q a -> Union r a
  projectAt :: Proxy n -> Union r a -> Maybe (q a)

instance MemberAt q (q ': r) 'Z where
  injectAt _ = Inject
  projectAt _ (Inject a) = Just a
  projectAt _ (Weaken _) = Nothing

instance (r ~ (r' ': rs),MemberAt q r n) => MemberAt q (x ': r) ('S n) where
  injectAt _ = Weaken . injectAt (Proxy :: Proxy n)
  projectAt _ (Inject _) = Nothing
  projectAt _ (Weaken x) = projectAt (Proxy :: Proxy n) x

type family IndexOf (q :: * -> *) r :: N where
  IndexOf q (q ': r) = 'Z
  IndexOf q (_ ': r) = 'S (IndexOf q r)
  
type family Head (xs :: [x]) :: x where
  Head (x ': _) = x

type family Tail (xs :: [x]) :: [x] where
  Tail (_ ': xs) = xs

class (MemberAt q r (IndexOf q r), r ~ (Head r ': Tail r)) => Member q r where
  inject :: q a -> Union r a
  project :: Union r a -> Maybe (q a)

instance (MemberAt q r (IndexOf q r),r ~ (Head r ': Tail r)) => Member q r where
  inject = injectAt (Proxy :: Proxy (IndexOf q r))
  project = projectAt (Proxy :: Proxy (IndexOf q r))

extract :: Union '[q] a -> q a
extract (Inject qa) = qa
-- extract (Weaken _) = error "Even the typechecker knows this is impossible :P"

{-
--type Member q r = Index (IndexOf q r) r ~ q

class Member q r where
  inject :: q a -> Union r a
  project :: Union r a -> Maybe (q a)

instance ((IndexOf q r) ~ Z) => Member q r where
  inject = _
  project (Inject a) = _
  project (Weaken _) = _

instance Member q r => Member q (x ': r) where
  inject = _
  project (Inject _) = Nothing
  project (Weaken x) = _
-}
