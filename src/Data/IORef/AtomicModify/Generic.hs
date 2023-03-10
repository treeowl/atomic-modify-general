{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- Unsafe in the presence of custom Generic instances.
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ViewPatterns #-}

-- | Atomic modification for more general records, using GHC generics to check
-- their suitablility. When applicable, this is faster than the general
-- utilities in "Data.IORef.AtomicModify".
module Data.IORef.AtomicModify.Generic
  ( atomicModifyIORef2Native
  ) where

import Data.Kind (Constraint, Type)
import GHC.Generics
import GHC.IORef (IORef (..))
#if MIN_VERSION_base(4,13,0)
import GHC.STRef (STRef (..))
import GHC.Exts (atomicModifyMutVar2#)
#else
import Data.IORef.AtomicModify.Generic.UnsafeToPair (unsafeToPair, unsafeFromPair)
import Data.IORef.AtomicModify (atomicModifyIORef3General)
#endif
import GHC.TypeLits
import GHC.IO (IO (..))

-- This trickery was stolen from Csongor Kiss. I don't think we want to use
-- Generic itself, because we don't actually need the Generic dictionary.
-- We use a type family rather than a type synonym to support GHC <= 9.2,
-- which throw a "could not calculate" error on the synonym definition. Huh.
type family EnsureGenericData t where
  EnsureGenericData t = EnsureGeneric' t (Rep t)
    (TypeError ('Text "Could not calculate " :<>: 'ShowType (Rep t) :$$:
          'Text "Is it an instance of " :<>: 'ShowType Generic :<>: 'Text "?"))
type family EnsureGeneric' t (rep :: Type -> Type) err :: Constraint where
  EnsureGeneric' t (M1 _ ('MetaData _ _ _ 'True) f) _ = TypeError ('ShowType t :<>: 'Text " is a newtype.")
  EnsureGeneric' _ _ _ = ()

-- A generalization of 'GHC.IORef.atomicModifyIORef2' to any datatype (not
-- newtype) with exactly one constructor whose first field
--
-- 1. has the same type as the value in the 'IORef'.
-- 2. is not unpacked.
--
-- Note that the non-unpackedness criterion can potentially be susceptible
-- to change by various compiler flags! An invalid use might therefore
-- compile successfully with @-O0@ but produce a type error with @-O@ or
-- @-O2@. This can be exacerbated by @-funbox-strict-fields@. To ensure
-- type checking will succeed with a record whose first field is strict and
-- monomorphic, it is best to use @{-# NOUNPACK #-}@ explicitly.
--
-- This function uses the 'atomicModifyMutVar2#' primop, and will therefore be
-- faster than the more general
-- 'Data.IORef.AtomicModify.atomicModifyIORef2General', and more likely to
-- succeed in a reasonable amount of time when there is substantial contention
-- for the 'IORef'.
--
-- == WARNING
--
-- This function is safe when used with /derived/ 'Generic' instances. It may
-- be /very unsafe/ when used with hand-written ones. In particular, we use the
-- type's 'Generic' instance (solely) to determine whether its physical layout
-- is suitable for our purposes. We also rely on certain details of how GHC
-- represents values in memory, and particularly the fact that records are
-- represented using a consistent \"pointers-first\" layout with the first
-- field appearing first.
atomicModifyIORef2Native
  :: (EnsureGenericData t, FirstField t (Rep t) ~ a) => IORef a -> (a -> t) -> IO (a, t)
#if MIN_VERSION_base(4,13,0)
atomicModifyIORef2Native (IORef (STRef ref)) f = IO $ \s ->
  case atomicModifyMutVar2# ref f s of
    (# s', old, !r #) -> (# s', (old, r) #)
#else
atomicModifyIORef2Native ref f = do
  -- We don't use fst here because it doesn't inline properly in this context
  -- with old GHC versions.
  (old, _new, unsafeFromPair -> !r) <- atomicModifyIORef3General ref (\(a, _) -> a) (unsafeToPair . f)
  pure (old, r)
#endif

type family FirstField t rep where
  FirstField t (M1 _ ('MetaSel _ _ _ 'DecidedUnpack) f) =
    TypeError ('Text "The first field of " :<>: 'ShowType t :<>: 'Text " is unpacked")
  FirstField t (M1 i c f) = FirstField t f
  FirstField t (_ :+: _) = TypeError ('ShowType t :<>: 'Text " is not a record type")
  FirstField t (f :*: _) = FirstField t f
  FirstField _ (K1 i c) = c
  FirstField t V1 = TypeError ('ShowType t :<>: 'Text " has no constructors")
  FirstField t U1 = TypeError ('ShowType t :<>: 'Text " has no fields")
