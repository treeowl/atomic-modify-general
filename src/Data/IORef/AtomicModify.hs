{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Atomic 'IORef' and array modification operations for more general result
-- types.
module Data.IORef.AtomicModify
  ( atomicModifyIORef3General
  , atomicModifyArray3General
  , atomicModifySmallArray3General
  ) where

import GHC.IORef (IORef (..), newIORef, readIORef)
import GHC.STRef (STRef (..))
import GHC.Exts ( casMutVar#, MutVar#, RealWorld, readMutVar#, lazy, State#
                , writeMutVar#, Int (..), Int#, MutableArray#, readArray#
                , casArray#, SmallMutableArray#, readSmallArray#, casSmallArray#
                )
import Data.IORef.AtomicModify.SmallArraySize
                ( getSizeofSmallMutableArray# )
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Primitive.Array
import Data.Primitive.SmallArray
import Control.Monad (when)
import Control.Exception (ArrayException (..), throwIO)

-- | A version of 'GHC.IORef.atomicModifyIORef2' that takes an arbitrary pair
-- of functions. This function will allocate more than 'atomicModifyIORef2',
-- and will tend to take longer to succeed when there is a lot of contention
-- for the 'IORef'.
--
-- @
-- atomicModifyIORef2 ref f = do
--   (old, _new, r) <- atomicModifyIORef2General ref fst f
--   pure (old, r)
-- @
--
-- If the first function (the \"extraction function\") is a record field
-- selector (e.g., 'snd'), we do our best to make sure the thunk placed in the
-- 'IORef' is a selector thunk, so the garbage collector can drop the rest of
-- the record once the record is forced. In other cases, callers should
-- generally force the returned @new@ value in order to avoid a potential space
-- leak.
--
-- Conceptually:
--
-- @
-- atomicModifyIORef3General ref extract f = do
--   -- Begin atomic block
--   old <- 'readIORef' ref
--   let r = f old
--       new = extract r
--   'writeIORef' ref new
--   -- End atomic block
--   r `seq` pure (old, new, r)
-- @
--
-- where other threads cannot interfere with the operations in the \"atomic block\".
-- In particular, no other thread can write to the 'IORef' between the 'readIORef'
-- and the 'writeIORef' operations.
atomicModifyIORef3General
  :: IORef a -> (t -> a) -> (a -> t) -> IO (a, a, t)
atomicModifyIORef3General (IORef (STRef ref)) extract = \f -> do
  -- atomicModifyMutVar2# creates a thunk for the result of applying the user
  -- function 'f' to the "old" value read from the IORef, and then edits that thunk
  -- in a CAS loop. In Haskell land, thunks are immutable, so we can't exactly
  -- do that. Instead, we make an IORef, 'holder', to hold the "old" value, and
  -- use 'unsafeDupablePerformIO' to create a thunk that will read it and apply
  -- 'f' to the result. We /can/ edit the holder IORef in the CAS loop.
  -- Note: since casMutVar# introduces a full memory barrier, any thread reading
  -- the 'r' thunk from 'ref' will have "seen" the preceding 'writeMutVar' to 'holder',
  -- so it won't get the uninitialized value or anything similarly stale.
  holder@(IORef (STRef holder#)) <- newIORef uninitialized
  let r = unsafeDupablePerformIO (f <$> readIORef holder)
      -- I really don't think r is going to inline anyway, but if it does, we
      -- could produce an unnecessary space leak.
      {-# NOINLINE r #-}
  let new = extract r
  IO (\s -> case atomicModifyIORef3General' ref holder# new r s of
               (# s', old, new', !res #) -> (# s', (old, new', res) #))
{-# INLINE atomicModifyIORef3General #-}

atomicModifyIORef3General'
  :: MutVar# RealWorld a -> MutVar# RealWorld a -> a -> t -> State# RealWorld -> (# State# RealWorld, a, a, t #)
atomicModifyIORef3General' ref holder new r s1 =
  case readMutVar# ref s1 of { (# s2, old #) ->
  case writeMutVar# holder old s2 of { s3 ->
  case casMutVar# ref old new s3 of { (# s4, flag, _ #) ->
  case flag of
    -- Why the lazy invocations?
    --
    -- In the event that 'old' gets forced, unboxed, and reboxed between
    -- 'readMutVar#' and 'casMutVar#', the CAS will never succeed. I doubt
    -- that could happen anyway, but let's be sure.
    --
    -- If 'r' is forced before the holder is written, it will read an
    -- uninitialized value and throw an exception. Ouch. Let's make sure that
    -- doesn't happen either.
    0# -> (# s4, lazy old, lazy new, lazy r #)
    _ -> atomicModifyIORef3General' ref holder new r s4 }}}
{-# NOINLINE atomicModifyIORef3General' #-}

uninitialized :: a
uninitialized = error "Uninitialized. This is a bug in atomic-modify-generics."
{-# NOINLINE uninitialized #-}

-- | A version of 'atomicModifyIORef3General' for 'Array's. See the
-- documentation there. Indexing is performed safely.
atomicModifyArray3General
  :: MutableArray RealWorld a -> Int -> (t -> a) -> (a -> t) -> IO (a, a, t)
-- See atomicModifyIORef3General for implementation comments.
-- Why do we perform safe indexing? This operation is expensive enough
-- that I don't think we really have to worry about the cost of a single
-- bounds check.
atomicModifyArray3General mary@(MutableArray mary#) ix@(I# ix#) extract = \f -> do
  -- We use unsigned comparison to make sure ix is non-negative
  -- and less than the array size with just one comparison. The LLVM
  -- backend is clever enough to produce this from the obvious two-sided
  -- check, but last I looked the native code generator wasn't.
  let !sz = sizeofMutableArray mary
  when ((fromIntegral ix :: Word) >= fromIntegral sz) $ outOfBoundsArray ix sz
  holder@(IORef (STRef holder#)) <- newIORef uninitialized
  let r = unsafeDupablePerformIO (f <$> readIORef holder)
      {-# NOINLINE r #-}
  let new = extract r
  IO (\s -> case atomicModifyArray3General' mary# ix# holder# new r s of
               (# s', old, new', !res #) -> (# s', (old, new', res) #))
{-# INLINE atomicModifyArray3General #-}

outOfBoundsArray :: Int -> Int -> IO a
outOfBoundsArray ix sz
  | ix < 0 = throwIO . IndexOutOfBounds $
      "\natomicModifyArray3General was passed a negative array index of " ++
        show ix ++ "."
  | otherwise = throwIO . IndexOutOfBounds $
      "\natomicModifyArray3General was passed an array index of " ++
        show ix ++ ",\nbut an array of only " ++ show sz ++ " elements."
{-# NOINLINE outOfBoundsArray #-}

atomicModifyArray3General'
  :: MutableArray# RealWorld a -> Int# -> MutVar# RealWorld a -> a -> t -> State# RealWorld -> (# State# RealWorld, a, a, t #)
atomicModifyArray3General' mary ix holder new r s1 =
  case readArray# mary ix s1 of { (# s2, old #) ->
  case writeMutVar# holder old s2 of { s3 ->
  case casArray# mary ix old new s3 of { (# s4, flag, _ #) ->
  case flag of
    0# -> (# s4, lazy old, lazy new, lazy r #)
    _ -> atomicModifyArray3General' mary ix holder new r s4 }}}
{-# NOINLINE atomicModifyArray3General' #-}

-- | A version of 'atomicModifyIORef3General' for 'SmallArray's. See the
-- documentation there. Indexing is performed safely.
atomicModifySmallArray3General
  :: SmallMutableArray RealWorld a -> Int -> (t -> a) -> (a -> t) -> IO (a, a, t)
-- See atomicModifyIORef3General for implementation comments.
atomicModifySmallArray3General (SmallMutableArray mary#) ix@(I# ix#) extract = \f -> do
  IO (\s -> case getSizeofSmallMutableArray# mary# s of (# s', sz# #) -> (# s', I# sz# #))
    >>= \sz -> when ((fromIntegral ix :: Word) >= fromIntegral sz) (outOfBoundsSmallArray ix sz)
  holder@(IORef (STRef holder#)) <- newIORef uninitialized
  let r = unsafeDupablePerformIO (f <$> readIORef holder)
      {-# NOINLINE r #-}
  let new = extract r
  IO (\s -> case atomicModifySmallArray3General' mary# ix# holder# new r s of
               (# s', old, new', !res #) -> (# s', (old, new', res) #))
{-# INLINE atomicModifySmallArray3General #-}

atomicModifySmallArray3General'
  :: SmallMutableArray# RealWorld a -> Int# -> MutVar# RealWorld a -> a -> t -> State# RealWorld -> (# State# RealWorld, a, a, t #)
atomicModifySmallArray3General' mary ix holder new r s1 =
  case readSmallArray# mary ix s1 of { (# s2, old #) ->
  case writeMutVar# holder old s2 of { s3 ->
  case casSmallArray# mary ix old new s3 of { (# s4, flag, _ #) ->
  case flag of
    0# -> (# s4, lazy old, lazy new, lazy r #)
    _ -> atomicModifySmallArray3General' mary ix holder new r s4 }}}
{-# NOINLINE atomicModifySmallArray3General' #-}

outOfBoundsSmallArray :: Int -> Int -> IO a
outOfBoundsSmallArray ix sz
  | ix < 0 = throwIO . IndexOutOfBounds $
      "\natomicModifySmallArray3General was passed a negative array index of " ++
        show ix ++ "."
  | otherwise = throwIO . IndexOutOfBounds $
      "\natomicModifySmallArray3General was passed an array index of " ++
        show ix ++ ",\nbut an array of only " ++ show sz ++ " elements."
{-# NOINLINE outOfBoundsSmallArray #-}
