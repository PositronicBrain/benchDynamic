% A comparison of Dynamic programming implementations
% Federico Squartini

> {-# LANGUAGE ForeignFunctionInterface #-}

> module Main where

> import Prelude hiding (read)
> import qualified Prelude as P (read)


> import Control.Monad (forM_)
> import Control.Monad.ST
> import Control.DeepSeq (deepseq)

> import Criterion.Main
> import Criterion.Config

> import qualified Data.IntMap as I
> import           Data.IntMap (empty,insert)

> import qualified Data.Vector as V (generate,unsafeIndex)
> import           Data.Vector.Generic (unsafeFreeze)
> import           Data.Vector.Unboxed.Mutable (unsafeRead,new,unsafeWrite)
> import qualified Data.Vector.Unboxed as U

> import Foreign.C.Types

> fibList :: Int -> Int
> fibList = deepseq fs . (fs !!)
>     where
>       fs = 1:1:zipWith (+) fs (tail fs)


> fibVector :: Int -> Int
> fibVector n = deepseq fs (fs `V.unsafeIndex` n)
>     where
>       fs = V.generate (n+1) $ \i -> case i of
>                                0 -> 1
>                                1 -> 1
>                                _ -> fs `V.unsafeIndex` (i-1) + fs `V.unsafeIndex` (i-2)


> fibIntmap :: Int -> Int
> fibIntmap n = deepseq fs (fs I.! n)
>     where
>       fs = foldr (\i -> insert i $ fs I.! (i-1) + fs I.! (i-2))
>                  (insert 1 1 $ insert 1 0 empty) [2..]

> fibUVector :: Int -> Int
> fibUVector n = (U.! n) $ runST $ do
>   fs <- new $ n+1
>   unsafeWrite fs 0 1
>   unsafeWrite fs 1 1
>   forM_ [2..n] $ \i -> do
>       a <- unsafeRead fs $ i-1
>       b <- unsafeRead fs $ i-2
>       unsafeWrite fs i $ a+b
>   unsafeFreeze fs


> foreign import ccall unsafe "fib" c_fib :: CInt -> CInt

> main :: IO ()
> main = do
>   let n = 1000000
>       myConfig = defaultConfig {cfgPerformGC = ljust True}
>   defaultMainWith myConfig (return ()) [
>         -- bench ("fib List " ++ show n) $ whnf fibList n,
>         -- bench ("fib IntMap " ++ show n) $ whnf fibIntmap n,
>         bench ("fib Vector " ++ show n) $ whnf fibVector n,
>         bench ("fib UVector " ++ show n) $ whnf fibUVector n,
>         bench ("fib C " ++ show n) $ whnf c_fib (fromIntegral n)
>         ]
