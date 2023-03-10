{-# language DeriveGeneric #-}
module Main (main) where
import Data.IORef
import Data.IORef.AtomicModify.Generic
import Control.Monad
import GHC.Generics

data Pair a b = Pair a b
  deriving (Show, Generic)

main :: IO ()
main = do
  ref <- newIORef (1 :: Int)
  replicateM_ 5 $ atomicModifyIORef2Native ref (\i -> Pair (i + 1) i) >>= print
