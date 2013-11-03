
-- | Wavy internal module.
module Data.Sound.Internal (
   -- * Types
   Time
 , Sound (..)
 , Word32
   -- * Functions
 , duration
 , atSample
 , sampleTime
 , timeSample
 , sample
 , mapSound , mapSoundAt
 , fromFunction
 , soundError
 , zeroChunks
 ) where

import Data.Word (Word32)
import Data.Sound.Core.Chunked
import Data.List (unfoldr)
import Control.DeepSeq
import Data.Maybe (fromMaybe)
import Data.Ratio

-- | Time is represented using 'Double's, though they are supposed to be always greater or equal to zero.
--   /Wavy/ will assume it, so, please, always provide non-negative times.
type Time = Double

-- | Sounds are represented by this type. The representation is sample rate dependent,
--   meaning that sounds with different sample rates cannot be operated as if they were
--   of the same class of objects.
--
--   After creating a sound, you may want to encode it using one of the supported encoding
--   formats. Look at these modules to do so:
--
--   * "Data.Sound.WAVE"
--
--   They also support decoding, so you can also get a 'Sound' from an encoded data, possibly
--   coming from an actual sound file.
data Sound = S { rate     :: !Int     -- ^ Sample rate.
               , nSamples :: !Int     -- ^ Number of samples.
               , channels :: !Int     -- ^ Number of channels.
               , schunks  ::  Chunked -- ^ Sequence of samples.
                 }

-- | Internal function to throw errors giving information about
--   the sounds that arised the error.
soundError :: [Sound] -- ^ Sounds the error is related with.
           -> String  -- ^ Name of the failing process.
           -> String  -- ^ Explanation of the error.
           -> a
soundError ss n e = error $ unwords $ 
    [ n ++ ":" ]
 ++ fmap (\s -> concat [ "(" , show (channels s) , "@" , show (rate s) , ")" ] ) ss
 ++ [ " " ++ e ]

-- NFData instance

instance NFData Sound where
 rnf (S _ _ _ c) = rnf c

--

-- | Try to get a sample. If it is out of the range of the given sound,
--   it returns the zero sample. This way, you can treat sounds as
--   infinite signals that are zero from some time to infinity.
atSample :: Int    -- ^ Sample index.
         -> Sound  -- ^ Sound containing the sample.
         -> Sample
atSample n (S _ _ nc cs) = fromMaybe (multiSample nc 0) $ cs ! n

{-# RULES
"sound/samples1" forall r i. timeSample r (sampleTime r i) = i
"sound/samples2" forall r t. sampleTime r (timeSample r t) = t
  #-}

-- | Calculates the time when a sample at a certain index occurs.
sampleTime :: Int  -- ^ Sample rate.
           -> Int  -- ^ Sample index.
           -> Time -- ^ Time when the sample occurs.
sampleTime r n = fromIntegral n / fromIntegral r

-- | Calculates the sample index at a given time.
timeSample :: Int  -- ^ Sample rate.
           -> Time -- ^ Time when the sample occurs.
           -> Int  -- ^ Sample index.
timeSample r d = floor $ fromIntegral r * d

-- | Duration of a sound in seconds.
duration :: Sound -> Time
duration s = sampleTime (rate s) (nSamples s) 

-- | Look for a 'Sample' at a given time.
--
--   Since digital sounds are discrete (finite) signals, the approach of this
--   function is to give the nearest sample to the given time.
sample :: Sound  -- ^ Sound to take the sample from.
       -> Time   -- ^ Please, choose a non-negative number.
       -> Sample -- ^ The sample at the given time.
sample s t = atSample (timeSample (rate s) t) s

{-# RULES
"sound/mapAt" forall f g s. mapSoundAt f (mapSoundAt g s) = mapSoundAt (\i x -> g i (f i x)) s
"sound/map"   forall f g s. mapSound f (mapSound g s) = mapSound (f . g) s
  #-}

-- | Map a function over all samples in a given sound. The 'Word32' extra parameter
--   is the sample index of the 'Sample' you are modifying.
--
-- > mapSoundAt f (mapSoundAt g s) = mapSoundAt (\i x -> g i (f i x)) s
-- > mapSoundAt f (fromFunction r d p g) =
-- >   fromFunction r d p (\t -> let i = timeSample r t
-- >                             in  f i (g t))
--
mapSoundAt :: (Int -> Sample -> Sample)
           -> Sound -> Sound
mapSoundAt f s = s { schunks = mapChunked f (schunks s) }

-- | Map a function over all samples in a given sound.
--
-- > mapSound = mapSoundAt . const
-- > mapSound f (mapSound g s) = mapSound (f . g) s
-- > mapSound f (fromFunction r d p g) = fromFunction r d p (f . g)
--
mapSound :: (Sample -> Sample)
         -> Sound -> Sound
mapSound = mapSoundAt . const

{-# RULES
"sound/mapFunctionAt" forall f r d p g. mapSoundAt f (fromFunction r d p g) =
    fromFunction r d p (\t -> let i = timeSample r t
                              in  f i (g t))
"sound/mapFunction" forall f r d p g. mapSound f (fromFunction r d p g) =
    fromFunction r d p (f . g)
  #-}

-- | Create a sound from a generator function 'Time' @->@ 'Sample'.
--
fromFunction :: Int              -- ^ Sample rate.
             -> Time             -- ^ Total duration.
             -> Maybe Time       -- ^ If your function is periodic, use this argument to indicate the period.
                                 --   Doing so you improve the performance of calling this function since it will
                                 --   only calculate one period and then repeat it.
             -> (Time -> Sample) -- ^ Generator function.
             -> Sound            -- ^ Resulting sound.
{-# INLINE[1] fromFunction #-}
fromFunction rt d (Just p) f = S rt n nc $ chunkedFromList n $ cycle xs
 where
  n  = timeSample rt d
  -- Dealing with the period problem.
  (nP,r_) = properFraction (fromIntegral rt * p)
  r = toRational r_
  nc = fromIntegral $ sampleLength $ f 0
  g = f . sampleTime rt
  basicList = fmap g [1..nP]
  addList   = fmap g [ nP + 1 .. nP + fromIntegral (numerator r) ]
  xs = concat (replicate (fromIntegral $ denominator r) basicList) ++ addList
fromFunction rt d Nothing f = S rt n nc $ chunkedFromList n $
  unfoldr (\i -> if i > n then Nothing
                          else let y = g i
                               in  Just (y,i+1)) 1
 where
  n = timeSample rt d
  nc = fromIntegral $ sampleLength $ f 0
  g = f . sampleTime rt

-- | Create 'Chunks' with the given number of samples, each sample
--   with the given number of channels, and every channel filled with zeroes.
zeroChunks :: Int -- ^ Number of samples.
           -> Int -- ^ Number of channels.
           -> Chunked
{-# INLINE zeroChunks #-}
zeroChunks n nc = chunkedFromList n $ repeat $ multiSample nc 0
