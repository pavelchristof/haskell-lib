{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  Control.Monad.UndoST
Description :  A revertible, strict state-transformer monad.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

The UndoST monad is a strict ST monad that can be reverted.
It could be nice to move STRef and STArray related functions to their own modules. 
Also some functions and typeclass instances are still missing.
-}

module Control.Monad.UndoST where

import Control.Monad.ST
import Data.STRef
import Data.Array.MArray

-- The revertible lazy state-transformer monad. A computation of type UndoST s a transforms an internal state indexed
-- by s, and returns a value of type a and a new computation of type ST s () that reverts any changes made to the state.
newtype UndoST s a = UndoST { runUndoST :: ST s (a, ST s ()) }

instance Monad (UndoST s) where
    return a = UndoST $ return (a, return ())
    {-# INLINE return #-}
    
    f >>= g = UndoST $ do
        (a, c1) <- runUndoST f
        (b, c2) <- runUndoST (g a)
        return (b, c2 >> c1)
    {-# INLINE (>>=) #-}

-- Run the UndoST monad and return its result value.
evalUndoST :: UndoST s a -> ST s a
evalUndoST st = do
    (val, _) <- runUndoST st
    return val
{-# INLINE evalUndoST #-}

-- Run the UndoST monad and return a computation that will revert state modifications.
execUndoST :: UndoST s a -> ST s (ST s ())
execUndoST st = do
    (_, undo) <- runUndoST st
    return undo
{-# INLINE execUndoST #-}

-- Builds a "pure" computation of type ST s a that transforms the state to compute a value of type a and then reverts 
-- all modifications made to the state, resulting in no external side effects.
pureST :: UndoST s a -> ST s a
pureST st = do
    (val, undo) <- runUndoST st
    undo
    return val
{-# INLINE pureST #-}

-- Lifts a "pure" ST computation, i.e. one that doesn't modify any existing refs (but can create new ones!).
liftPureST :: ST s a -> UndoST s a
liftPureST st = UndoST $ do
    val <- st
    return (val, return ())
{-# INLINE liftPureST #-}

-- newSTRef lifted to the UndoST monad.
newUndoSTRef :: a -> UndoST s (STRef s a)
newUndoSTRef a = liftPureST $ newSTRef a
{-# INLINE newUndoSTRef #-}

-- readSTRef lifted to the UndoST monad.
readUndoSTRef :: STRef s a -> UndoST s a
readUndoSTRef ref = liftPureST $ readSTRef ref
{-# INLINE readUndoSTRef #-}

-- Writes a new value into an STRef and returns a computation that restores the old value.
writeUndoSTRef :: STRef s a -> a -> UndoST s ()
writeUndoSTRef ref a = UndoST $ do
    old <- readSTRef ref
    writeSTRef ref a
    return ((), writeSTRef ref old)
{-# INLINE writeUndoSTRef #-}

-- Modifies an STRef and returns a computation that restores the old value.
modifyUndoSTRef :: STRef s a -> (a -> a) -> UndoST s ()
modifyUndoSTRef ref f = UndoST $ do
    old <- readSTRef ref
    modifySTRef ref f
    return ((), writeSTRef ref old)
{-# INLINE modifyUndoSTRef #-}

-- newArray lifted to the UndoST monad.
newUndoArray :: (MArray a e (ST s), Ix i) => (i, i) -> e -> UndoST s (a i e)
newUndoArray i e = liftPureST $ newArray i e
{-# INLINE newUndoArray #-}

-- newArray_ lifted to the UndoST monad.
newUndoArray_ :: (MArray a e (ST s), Ix i) => (i, i) -> UndoST s (a i e)
newUndoArray_ i = liftPureST $ newArray_ i
{-# INLINE newUndoArray_ #-}

-- readArray lifted to the UndoST monad.
readUndoArray :: (MArray a e (ST s), Ix i) => a i e -> i -> UndoST s e
readUndoArray arr i = liftPureST $ readArray arr i
{-# INLINE readUndoArray #-}

-- Writes an element in an array and returns a computation that restores the old element.
writeUndoArray :: (MArray a e (ST s), Ix i) => a i e -> i -> e -> UndoST s ()
writeUndoArray arr i e = UndoST $ do
    old <- readArray arr i
    writeArray arr i e
    return ((), writeArray arr i old)
{-# INLINE writeUndoArray #-}
