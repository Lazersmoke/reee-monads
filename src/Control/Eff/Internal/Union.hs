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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Eff.Internal.Union (Union(..),Member,project,projectUnique,inject,injectUnique,extract) where

data Union (r :: [* -> *]) a where
  Inject :: q a -> Union (q ': r) a
  Weaken :: Union (r ': s) a -> Union (x ': r ': s) a

data N = Z | S N

class MemberAt (n :: N) q r where
  injectAt :: forall a. q a -> Union r a
  projectAt :: forall a. Union r a -> Maybe (q a)

instance MemberAt 'Z q (q ': r) where
  injectAt = Inject
  projectAt (Inject a) = Just a
  projectAt (Weaken _) = Nothing

instance (r ~ (r' ': rs),MemberAt n q r) => MemberAt ('S n) q (x ': r) where
  injectAt = Weaken . injectAt @n
  projectAt (Inject _) = Nothing
  projectAt (Weaken x) = projectAt @n x

-- Begin type wizardry 
-- Credit to:
-- https://github.com/AndrasKovacs/misc-stuff/blob/master/haskell/Eff/EffInference.hs
data Entry = Apply | forall x. Single x

-- Preord turns a type into a unary application list version of itself
-- Turns `Either Int String` into '[Apply,Apply,Either,Int,String]
-- Turns `Maybe [Cat]` into '[Apply,Maybe,Apply,List,Cat]
type family Preord (x :: k) :: [Entry] where
  Preord (f x) = 'Apply ': (Preord f ++ Preord x)
  Preord x = '[ 'Single x]

-- Map Preord xs
type family PreordList (xs :: [a]) :: [[Entry]] where
  PreordList '[] = '[]
  PreordList (x ': xs) = Preord x ': PreordList xs

-- Annotate each element with an index
-- Elements will get shuffled by Narrow, so we must maintain the original index this way
type family IndexAnn (xs :: [a]) (i :: N) :: [(N,a)] where
  IndexAnn '[] _ = '[]
  IndexAnn (x ': xs) i = '(i,x) ': IndexAnn xs ('S i)

-- Filter for containing the entry
type family Narrow (e :: Entry) (xs :: [(N,[Entry])]) :: [(N,[Entry])] where
  -- Filtering [] is []
  Narrow _ '[] = '[]
  -- If the search entry and the current one match, add to list, otherwise, throw it out
  Narrow e ('(i,e' ': es) ': ess) = If (e == e') '[ '(i,es)] '[] ++ Narrow e ess

type family FindPreord (es :: [Entry]) (ess :: [(N,[Entry])]) :: N where
  -- If there is only one item left after all the narrowings, it must be the right one
  FindPreord _ '[ '(i,_)] = i
  -- Filter out the ones that don't match. Get stuck if we have multiple matches or none
  FindPreord (e ': es) ess = FindPreord es (Narrow e ess)

type Find x ys = FindPreord (Preord x) (IndexAnn (PreordList ys) 'Z)

type family If (b :: Bool) (t :: k) (f :: k) :: k where
  If 'True t _ = t
  If 'False _ f = f

type family (x :: k) == (y :: j) :: Bool where
  x == x = 'True
  _ == _ = 'False

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

type family Head (xs :: [x]) :: x where
  Head (x ': _) = x

type family Tail (xs :: [x]) :: [x] where
  Tail (_ ': xs) = xs

type family SimpleFind (x :: a) (xs :: [a]) :: N where
  SimpleFind x (x : _) = 'Z
  SimpleFind x (_ : xs) = 'S (SimpleFind x xs)

type UniqueMember q r = MemberAt (Find q r) q r
type Member q r = MemberAt (SimpleFind q r) q r

{-# INLINE inject #-}
inject :: forall q r a. Member q r => q a -> Union r a
inject = injectAt @(SimpleFind q r)

{-# INLINE injectUnique #-}
injectUnique :: forall q r a. UniqueMember q r => q a -> Union r a
injectUnique = injectAt @(Find q r)

{-# INLINE project #-}
project :: forall q r a. Member q r => Union r a -> Maybe (q a)
project = projectAt @(SimpleFind q r)

{-# INLINE projectUnique #-}
projectUnique :: forall q r a. UniqueMember q r => Union r a -> Maybe (q a)
projectUnique = projectAt @(Find q r)

{-# INLINE extract #-}
extract :: Union '[q] a -> q a
extract (Inject qa) = qa
-- extract (Weaken _) = error "Even the typechecker knows this is impossible :P"
