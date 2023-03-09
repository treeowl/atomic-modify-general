{-# language MagicHash #-}
{-# language UnboxedTuples #-}
module Data.IORef.AtomicModify.SmallArraySize
  ( getSizeofSmallMutableArray#
  ) where
import GHC.Exts (SmallMutableArray#, State#, RealWorld, Int#, sizeofSmallMutableArray#)

getSizeofSmallMutableArray# :: SmallMutableArray# s a -> State# RealWorld -> (# State# RealWorld, Int# #)
getSizeofSmallMutableArray# sma s = (# s, sizeofSmallMutableArray# sma #)
