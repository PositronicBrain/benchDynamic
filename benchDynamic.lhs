% A comparison of Dynamic programming implementations
% Federico Squartini

> {-# LANGUAGE ForeignFunctionInterface #-}

> module Main where

> import Prelude hiding (read)
> import qualified Prelude as P (read)
> import Data.List (foldl')

> import Control.Monad (forM_)
> import Control.Monad.ST
> import Control.DeepSeq (deepseq,force)

> import Criterion.Main
> import Criterion.Config

> import qualified Data.IntMap.Strict as I

> import qualified Data.Map.Strict as M


> import qualified Data.Vector as V ((!),generate,unsafeIndex)
> import qualified Data.Vector.Mutable as VM (unsafeRead,new,unsafeWrite)
> import           Data.Vector.Generic (unsafeFreeze)
> import qualified Data.Vector.Unboxed.Mutable as UM (unsafeRead,new,unsafeWrite)
> import qualified Data.Vector.Unboxed as U



> import Foreign.C.Types

> fibList :: Int -> Int
> fibList n = last $ force (take n fs)
>     where
>       fs = 1:1:zipWith (+) fs (tail fs)


> fibVector :: Int -> Int
> fibVector n = deepseq fs $ fs `V.unsafeIndex` n
>     where
>       fs = V.generate (n+1) $ \i -> case i of
>                                0 -> 1
>                                1 -> 1
>                                _ -> fs `V.unsafeIndex` (i-1) + fs `V.unsafeIndex` (i-2)


> fibIntmap :: Int -> Int
> fibIntmap n = (I.! n) $ foldl' step I.empty [0..n]
>    where
>      step m 0 = I.insert 0 1 m
>      step m 1 = I.insert 1 1 m
>      step m i = I.insert i (m I.! (i-1) + m I.! (i-2)) m
>      {-# INLINE step #-}

> fibMap :: Int -> Int
> fibMap n = (M.! n) $ foldl' step M.empty [0..n]
>    where
>      step m 0 = M.insert 0 1 m
>      step m 1 = M.insert 1 1 m
>      step m i = M.insert i (m M.! (i-1) + m M.! (i-2)) m
>      {-# INLINE step #-}


> fibMVector :: Int -> Int
> fibMVector n = deepseq fs $ fs `V.unsafeIndex` n
>     where
>       fs = runST $ do
>              v <- VM.new $ n+1
>              VM.unsafeWrite v 0 1
>              VM.unsafeWrite v 1 1
>              forM_ [2..n] $ \i -> do
>                  a <- VM.unsafeRead v $ i-1
>                  b <- VM.unsafeRead v $ i-2
>                  VM.unsafeWrite v i $ a+b
>              unsafeFreeze v


> fibMUVector :: Int -> Int
> fibMUVector n = (U.! n) $ runST $ do
>   fs <- UM.new $ n+1
>   UM.unsafeWrite fs 0 1
>   UM.unsafeWrite fs 1 1
>   forM_ [2..n] $ \i -> do
>       a <- UM.unsafeRead fs $ i-1
>       b <- UM.unsafeRead fs $ i-2
>       UM.unsafeWrite fs i $ a+b
>   unsafeFreeze fs


> foreign import ccall unsafe "fib" c_fib :: CInt -> CInt

> main :: IO ()
> main = do
>   let n = 1000000
>       myConfig = defaultConfig {cfgPerformGC = ljust True}
>   defaultMainWith myConfig (return ()) [
>         bench ("fib List " ++ show n) $ whnf fibList n,
>         bench ("fib Data.IntMap " ++ show n) $ whnf fibIntmap n,
>         bench ("fib Data.Map " ++ show n) $ whnf fibMap n,
>         bench ("fib Data.Vector (boxed) " ++ show n) $ whnf fibVector n,
>         bench ("fib Mutable UVector " ++ show n) $ whnf fibMUVector n,
>         bench ("fib Mutable Vector " ++ show n) $ whnf fibMVector n,
>         bench ("fib C " ++ show n) $ whnf c_fib (fromIntegral n)
>         ]
