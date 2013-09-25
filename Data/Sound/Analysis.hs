
module Data.Sound.Analysis (
    -- * Types
    Complex, Vector
    -- * Operations
  , hermitian, norm
    -- * N-th roots abelian group
  , zeta, basis, normalBasis
    -- * Fourier
  , fourierCoefs
  ) where

-- import Data.Word
import qualified Data.Complex as C
import qualified Data.Vector.Unboxed as A

type Complex = C.Complex Double

type Vector = A.Vector Complex

-- | N-th roots of unity.
zeta :: Int -- ^ Modulo
     -> Vector
zeta n = A.generate n $ \k -> C.cis $ 2*pi*(fromIntegral k/fromIntegral n)

basis :: Int -- ^ Modulo
      -> [Vector]
basis n = [ A.map (**fromIntegral l) $ zeta n | l <- [ 0 .. n-1 ] ]

hermitian :: Vector -> Vector -> Complex
hermitian f g = A.sum $ A.zipWith (\x y -> x * C.conjugate y) f g

norm :: Vector -> Complex
norm f = sqrt $ hermitian f f

normalBasis :: Int -- ^ Modulo
            -> [Vector]
normalBasis n = fmap (A.map (/(sqrt $ fromIntegral n))) $ basis n

-- | Fourier coefficients of a complex-valued function on Z(N).
fourierCoefs :: Vector -> Vector
fourierCoefs f = A.generate n $ \i -> recip (fromIntegral n) * hermitian f (basis n !! i)
  where
    n = A.length f

-- | Build a complex-valued function on Z(N) from its Fourier coefficients.
fourierBuild :: Vector -> Vector
fourierBuild cs = A.generate n $ \k -> A.sum $ A.zipWith (*) cs $ basis n !! k
  where
    n = A.length cs

{- Bibliography

[1] Fourier Analysis, an introduction. Elias M. Stein and Rami Shakarchi.

-}
