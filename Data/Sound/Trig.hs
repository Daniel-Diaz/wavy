
module Data.Sound.Trig (
    -- * @I@ type
    I
    -- * Conversion
  , fromI, toI
  ) where

-- | The type of the numbers in the (-1,1) /open/ interval.
--
--   If /i/ belongs to (-1,1) then it can be written as
--
-- > i = lambda * atan x
-- >   where
-- >     lambda = 2/pi
--
--   where /x/ is a real number. This is the representation
--   that 'I' uses internally.
newtype I = I Double

lambda :: Double
lambda = 2/pi

-- | Cast from 'I' to 'Double', therefore always getting
--   a number within the (-1,1) interval.
fromI :: I -> Double
fromI (I x) = lambda * atan x

-- | Cast from 'Double' to 'I'. The argument should be in the
--   (-1,1) interval. Otherwise, it will be traslated to that
--   interval adding an integer multiple of 2 (the length of
--   the interval).
toI :: Double -> I
toI y = I $ tan (y/lambda)

instance Show I where
  show = show . fromI

instance Num I where
  fromInteger = I . fromInteger
  (I x) + (I x') = I $ x + x'
  (I x) * (I x') = I $ x * x'
  (I x) - (I x') = I $ x - x'
  negate (I x) = I $ negate x
  abs (I x) = I $ abs x
  signum (I x) = I $ signum x
