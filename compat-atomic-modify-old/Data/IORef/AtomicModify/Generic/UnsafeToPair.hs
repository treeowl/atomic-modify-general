{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fno-worker-wrapper #-}


-- | The documentation for `unsafeCoerce` firmly warns that it should not be
-- used to coerce between actually different types. But we want to produce
-- selector thunks and we don't want to rely on Generic nonsense inlining away
-- to do so. So ... we use `unsafeCoerce` in an entirely illegitimate way. We
-- use this module to make sure our bogus coercions don't escape and cause
-- trouble in the optimizer. Using `-fno-worker-wrapper` and `NOINLINE` pragmas
-- prevents GHC from producing unfoldings for these hideous functions, so
-- they're quite opaque. However, it still produces demand signatures and arity
-- info, which *tend* to be good to have right. I'm not sure how much they
-- matter in this particular case.
module Data.IORef.AtomicModify.Generic.UnsafeToPair
  ( unsafeToPair
  , unsafeFromPair
  ) where

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- | Pretend that an arbitrary type is actually a pair whose first
-- component is whatever we want it to be. This is very unsafe!
unsafeToPair :: t -> (a, Any)
unsafeToPair = unsafeCoerce
{-# NOINLINE unsafeToPair #-}

-- | Coerce (back) from a (fake) pair to the actual type.
unsafeFromPair :: (a, Any) -> t
unsafeFromPair = unsafeCoerce
{-# NOINLINE unsafeFromPair #-}
