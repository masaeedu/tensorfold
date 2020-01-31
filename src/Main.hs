{-# LANGUAGE DeriveFunctor, ImpredicativeTypes, UndecidableInstances, AllowAmbiguousTypes #-}
module Main where

import Data.Functor.Const (Const(..))

-- Folding a tensor along a type level list

data Fold1 :: (* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> [* -> *] -> * -> *
  where
  Base1 :: z a -> Fold1 z t '[] a
  Append1 :: t x (Fold1 z t xs) a -> Fold1 z t (x ': xs) a

instance Functor z => Functor (Fold1 z t '[])
  where
  fmap f (Base1 za) = Base1 (fmap f za)

instance Functor (t x (Fold1 z t xs)) => Functor (Fold1 z t (x ': xs))
  where
  fmap f (Append1 txr) = Append1 (fmap f txr)

-- Get the Union type

data VoidF a
  deriving Functor

data (f :+: g) a = L (f a) | R (g a)
  deriving Functor

type Union = Fold1 VoidF (:+:)

-- Get the HList type

type UnitF = Const ()

data (f :*: g) a = (:*:) { fst :: f a, snd :: g a }
  deriving Functor

type HList = Fold1 UnitF (:*:)

-- Test it out

foo :: Union '[Const String, Maybe, []] Int
foo = Append1 $ R $ Append1 $ L $ Just 1

foo' :: Union '[Const String, Maybe, []] Int
foo' = fmap (* 2) foo

bar :: HList '[Const String, Maybe, []] Int
bar =
  Append1 $ (:*:) (Const "foo") $
  Append1 $ (:*:) (Just 1) $
  Append1 $ (:*:) [1, 2, 3] $
  Base1 (Const ())

bar' :: HList '[Const String, Maybe, []] Int
bar' = fmap (* 2) bar

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- Other notes
-- The tensors are usually bifunctors, often also monoidal structures, and sometimes the product tensor in some category
type (f ~> g) = forall x. f x -> g x

class HBifunctor t
  where
  hbimap :: (a ~> c) -> (b ~> d) -> t a b ~> t c d

instance HBifunctor (:+:)
  where
  hbimap _ n (R ga) = R (n ga)
  hbimap n _ (L fa) = L (n fa)

-- monoidal structure ...
-- product ...

-- Can we unify Fold0 below and Fold1 in Haskell somehow?
data Fold0 :: * -> (* -> * -> *) -> [*] -> *
  where
  Base0 :: z -> Fold0 z t '[]
  Append0 :: t x (Fold0 z t xs) -> Fold0 z t (x ': xs)
