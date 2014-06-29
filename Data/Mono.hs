{-# LANGUAGE DefaultSignatures #-}
{- |
Module      :  Data.Mono
Description :  Monomorphic functor, foldable, traversable and monad.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

It turns out that Control.Lens does exactly what this module was supposed to do and does it better.
-}

module Data.Mono where

import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import Data.Functor.Constant
import Control.Applicative

class MonoFunctor f e where
    omap :: (e -> e) -> f -> f
    default omap :: MonoTraversable f e => (e -> e) -> f -> f
    omap f = runIdentity . otraverse (Identity . f)

class MonoFoldable f e where
    -- | Map each element of the structure to a monoid, and combine the results.
    ofoldMap :: Monoid m => (e -> m) -> f -> m
    default ofoldMap :: (MonoTraversable f e, Monoid m) => (e -> m) -> f -> m
    ofoldMap f = getConstant . otraverse (Constant . f)

    -- | Right-associative fold of a structure.
    ofoldr :: (e -> b -> b) -> b -> f -> b
    ofoldr f z t = appEndo (ofoldMap (Endo . f) t) z

    -- | Right-associative fold of a structure, but with strict application of the operator.
    ofoldr' :: (e -> b -> b) -> b -> f -> b
    ofoldr' f z0 xs = ofoldl f' id xs z0
      where f' k x z = k $! f x z

    -- | Left-associative fold of a structure.
    ofoldl :: (b -> e -> b) -> b -> f -> b
    ofoldl f z t = appEndo (getDual (ofoldMap (Dual . Endo . flip f) t)) z

    -- | Left-associative fold of a structure, but with strict application of the operator.
    ofoldl' :: (b -> e -> b) -> b -> f -> b
    ofoldl' f z0 xs = ofoldr f' id xs z0
      where f' x k z = k $! f z x

    -- | A variant of 'foldr' that has no base case, and thus may only be applied to non-empty structures.
    ofoldr1 :: (e -> e -> e) -> f -> e
    ofoldr1 f xs = fromMaybe (error "ofoldr1: empty structure")
                    (ofoldr mf Nothing xs)
      where
        mf x Nothing = Just x
        mf x (Just y) = Just (f x y)

    -- | A variant of 'foldl' that has no base case, and thus may only be applied to non-empty structures.
    ofoldl1 :: (e -> e -> e) -> f -> e
    ofoldl1 f xs = fromMaybe (error "ofoldl1: empty structure")
                    (ofoldl mf Nothing xs)
      where
        mf Nothing y = Just y
        mf (Just x) y = Just (f x y)

class (MonoFunctor t e, MonoFoldable t e) => MonoTraversable t e where
    -- | Map each element of a structure to an action, 
    -- evaluate these actions from left to right, and collect the results.
    otraverse :: Applicative f => (e -> f e) -> t -> f t

class MonoMonadBase f e where
    oreturn :: e -> f

class (MonoFunctor f e, MonoMonadBase f e) => MonoMonad m f e where
    (@>>=) :: m -> (e -> f) -> m
