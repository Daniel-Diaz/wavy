
module Data.Sound.Analysis (
    -- * Types
    Complex, Vector
    -- * Operations
  , hermitian, norm
    -- * N-th roots abelian group
  , zeta, basisf
    -- * Fourier
  , fourierTransform
  , fourierInverse
    -- * Utils
  , splitVector
  , unsplitVector
  , vectorMakeComplex
  , vectorMakeReal
  ) where

-- import Data.Word
import qualified Data.Complex as C
import qualified Data.Vector.Unboxed as A
import qualified Data.Vector as V
import Data.List (foldl')
import Control.Parallel.Strategies

type Complex = C.Complex Double

type Vector = A.Vector Complex

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

-- | N-th roots of unity.
zeta :: Int -- ^ Modulo
     -> Vector
zeta n = A.generate n $ \k -> C.cis $ 2*pi*(fromIntegral k/fromIntegral n)

tau :: Double
tau = 2*pi

data BasisTree = Node1 (Int,Int,Int,Complex) BasisTree
               | Node2 (Int,Int,Int,Complex) BasisTree BasisTree
               | TreeEnd

basisf :: Int -> Int -> Int -> Complex
{-# INLINE basisf #-}
basisf l k n = C.cis $ (/fromIntegral n) $ tau * fromIntegral (l*k)

{-
basistree :: BasisTree
basistree = buildtree 0 0 1
  where
    buildnode1 l k n l' k' n' = Node1 (l,k,n,basisf l k n) (buildtree l' k' n')
    buildnode2 l k n l' k' n' l'' k'' n'' = Node2 (l,k,n,basisf l k n) (buildtree l'  k'  n')
                                                                       (buildtree l'' k'' n'')
    buildtree l k n
      | l == 0 && k == 0 = if n == 1
                              then buildnode1 l k n 0 0 2
                              else buildnode2 l k n 0 0 (n+1) 1 0 n
      | k == l = Node1 (l,k,n,basisf l k n) TreeEnd
      | l == (n-1) = buildnode1 l k n l (k+1) n
      | k == 0 = buildnode2 l k n (l+1) 0 n l 1 n
      | otherwise = buildnode1 l k n l (k+1) n

-- | This lookup is implemented only for the 'basistree'.
treelookup :: Int -> Int -> Int -> BasisTree -> Complex
treelookup l k n = go
  where
   go (Node1 (l',k',n',x) t) = if n == n' && l == l' && k == k' then x else go t
   go (Node2 (l',k',n',x) t1 t2)
     | n /= n' = go t1
     | l /= l' = if l' == 0 then go t2 else go t1
     | k /= k' = go t2
     | otherwise = x
   go TreeEnd = error "basistree: end reached!"

basis :: Int -> Int -> Int -> Complex
basis l k n = treelookup l k n basistree
-}

hermitian :: Vector -> Vector -> Complex
hermitian f g = go 0
  where
    -- Assumption: f and g have the same length.
    n = A.length f
    go i = if i == n then 0 else (f A.! i) * (C.conjugate $ g A.! i) + go (i+1)

norm :: Vector -> Complex
norm f = sqrt $ hermitian f f

-- | Fourier coefficients of a complex-valued function on Z(N).
--   Finite Fourier Transform (FFT).
fourierTransform :: Vector -> Vector
fourierTransform f
  | mod n 2 == 0 && n > 32
                 = runEval $ do
                     fE <- rpar $ fourierTransform $ A.generate n2 $ \i -> f A.! (2*i)
                     fO <- rpar $ fourierTransform $ A.generate n2 $ \i -> f A.! (2*i+1)
                     rdeepseq fE
                     rdeepseq fO
                     -- return $ A.generate n $ \i -> (/2) $ (fE A.! mod i n2) + (fO A.! mod i n2) * (basisf i 1 n)
                     return $ A.generate n $ \i ->
                       if i < n2
                          then (/2) $ (fE A.! i)        + (fO A.! i)        * recip (basisf i 1 n)
                          else (/2) $ (fE A.! (i - n2)) + (fO A.! (i - n2)) * recip (basisf i 1 n)
  | otherwise = A.generate n $ \i -> q * sum' [ (f A.! k) * (recip $ basisf k i n) | k <- [0 .. n-1] ]
    where
      n  = A.length f
      n2 = div n 2
      q  = recip $ fromIntegral n

-- | Build a complex-valued function on Z(N) from its Fourier coefficients.
--   Inverse FFT.
fourierInverse :: Vector -> Vector
fourierInverse cs = A.generate n $ \k -> foldl' (+) 0 $ [ (cs A.! i) * basisf i k n | i <- [0 .. n-1] ]
  where
    n = A.length cs

-- Utils

splitVector :: A.Unbox a => V.Vector (A.Vector a) -> [A.Vector a]
splitVector v = [ A.generate m (\j -> (v V.! j) A.! i) | i <- [0 .. n-1] ]
  where
    n = A.length (v V.! 0)
    m = V.length v

unsplitVector :: A.Unbox a => [A.Vector a] -> V.Vector (A.Vector a)
unsplitVector vs = V.generate (A.length $ head vs) $ \i -> A.fromList $ fmap (A.! i) vs

vectorMakeComplex :: A.Vector Double -> Vector
vectorMakeComplex = A.map (C.:+0)

vectorMakeReal :: Vector -> A.Vector Double
vectorMakeReal = A.map C.realPart

{- Bibliography

[1] Fourier Analysis, an introduction. Elias M. Stein and Rami Shakarchi.

-}
