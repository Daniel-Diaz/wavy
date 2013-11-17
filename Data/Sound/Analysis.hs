
{-# LANGUAGE TemplateHaskell #-}

module Data.Sound.Analysis (
    -- * Types
    Complex, Vector
    -- * Operations
  , hermitian, norm
    -- * N-th roots abelian group
  , basis
    -- * Fourier
  , fourierTransform
  , fourierInverse
    -- * Utils
  , splitVector
  , unsplitVector
  , vectorMakeComplex
  , vectorMakeReal
  ) where

import qualified Data.Complex as C
import qualified Data.Vector.Unboxed as A
import qualified Data.Vector as V
import Numeric.FFT.Vector.Unnormalized (dft,idft,run)
import Numeric.FFT.Vector.Plan (Plan, PlanType (..), planOfType, execute)
import Data.Sound.Core.Chunked (chunkSize)

type Complex = C.Complex Double

type Vector = A.Vector Complex

tau :: Double
tau = 2*pi

basis :: Int -> Int -> Int -> Complex
{-# INLINE basis #-}
basis l k n = C.cis $ (/fromIntegral n) $ tau * fromIntegral (l*k)

hermitian :: Vector -> Vector -> Complex
hermitian f g = go 0
  where
    -- Assumption: f and g have the same length.
    n = A.length f
    go i = if i == n then 0 else (f A.! i) * (C.conjugate $ g A.! i) + go (i+1)

norm :: Vector -> Complex
norm f = sqrt $ hermitian f f

chunkdft :: Plan Complex Complex
chunkdft = planOfType Exhaustive dft chunkSize

chunkidft :: Plan Complex Complex
chunkidft = planOfType Exhaustive idft chunkSize

-- | Fourier coefficients of a complex-valued function on Z(N).
--   Finite Fourier Transform (FFT).
fourierTransform :: Vector -> Vector
fourierTransform v = f . g $ v
  where
   n = A.length v
   q = recip $ fromIntegral n
   f = A.map (*q)
   g = if n == chunkSize then execute chunkdft else run dft

-- | Build a complex-valued function on Z(N) from its Fourier coefficients.
--   Inverse FFT.
fourierInverse :: Vector -> Vector
fourierInverse v = g v
  where
   n = A.length v
   g = if n == chunkSize then execute chunkidft else run idft

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
