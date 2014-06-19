
-- | Core sound module.
module Data.Sound (
   -- * Basic types
   Time, Sample
 , Sound
   -- * Basic functions
 , duration , rate
 , channels , nSamples
 , sample

   -- * Wave generators
   -- ** Basic wave generators
 , zeroSound , zeroSoundWith
 , sine      , sineWith     , sineRaw
 , sawtooth  , sawtoothWith , sawtoothRaw
 , square    , squareWith   , squareRaw
 , triangle  , triangleWith , triangleRaw
   -- ** Variable Frequency Basic wave generators
 , sineV     , sineVR
   -- ** Functional wave generators
 , fromFunction
   -- ** Other wave generators
 , noise   , noiseR
 , pnoise  , pnoiseR
 , karplus , karplusR

   -- * Sound operators
   -- ** Basic operators
 , (<.>) , (<+>) , (<|>)
   -- ** Other operators
 , parWithPan , addAt

   -- * Modifiers
 , addSilenceBeg , addSilenceEnd
 , velocity , mapSound
 , pan , scale
 , divide , multiply
 , left , right

   -- * Effects
 , echo
   -- * Utils
 , loop, trim, backwards
 , affineFunction
 , linearFunction
   -- * Experimental
 , fourierSieve
   ) where

import Data.Monoid
import Data.Sound.Internal
import Data.Sound.Core.Chunked
import Data.Sound.Analysis
import qualified Data.Vector.Unboxed as A
import qualified Data.Complex as C
-- Maybe
import Data.Maybe (catMaybes)
-- Random
import Random.MWC.Pure
-- Sequences
import qualified Data.Sequence as Seq

-- | Add a silence at the beginning of a sound.
addSilenceBeg :: Time -- ^ Duration of the silence.
              -> Sound -> Sound
addSilenceBeg d s = multiply n (zeroSoundWith r d) <.> s
 where
  r = rate s
  n = channels s

-- | Add a silence at the end of a sound.
addSilenceEnd :: Time -- ^ Duration of the silence.
              -> Sound -> Sound
addSilenceEnd d s = s <.> multiply n (zeroSoundWith r d)
 where
  r = rate s
  n = channels s

-- | /Addition of sounds/. If one sound is longer, the remainder will remain without any change.
--   There are some restriction to use this function.
--
--   * Both arguments must share the same /sample rate/.
--
--   * Both arguments must share the same /number of channels/.
(<+>) :: Sound -> Sound -> Sound
s1@(S r l nc c) <+> s2@(S r' l' nc' c')
 | r  /= r'  = soundError [s1,s2] "<+>" $ "Can't add sounds with different sample rates. "
                                       ++ "Please, consider to change the sample rate of one of them."
 | nc /= nc' = soundError [s1,s2] "<+>" $ "Can't add two sounds with different number of channels. "
                                       ++ "Please, consider to change the number of channels in one of them."
 | otherwise = S r (max l l') nc $
     if l == l' then zipChunkedSame (zipSamples (+)) c c'
                else zipChunked     (zipSamples (+)) c c'

-- | /Parallelization of sounds/. Often refered as the /par/ operator.
--   Applying this operator over two sounds will make them sound at the same
--   time, but in different channels. The sound at the left will be at left-most
--   channels, and the right one at the right-most channels.
--   There are some restriction to use this function.
--
--   * Both arguments must share the same /sample rate/.
--
(<|>) :: Sound -> Sound -> Sound
s1@(S r l nc c) <|> s2@(S r' l' nc' c')
 | r  /= r'  = soundError [s1,s2] "<|>" $ "Can't par sounds with different sample rates. "
                                       ++ "Please, consider to change the sample rate of one of them."
 | otherwise = let c'' = if l < l' then zipChunkedSame appendSamples (c <> zeroChunks (l'-l) nc) c'
                                   else zipChunkedSame appendSamples c (c' <> zeroChunks (l-l') nc')
               in  S r (max l l') (nc+nc') c''

{- About the associativity of the sequencing operator.

If we are using balaced chunk appending, the sequencing operator (<.>) should be
left associative (infixl). Suppose we have three sound chunks of size n. When we
append two chunks, the right chunk gets balanced (unless it is already balanced)
in order to get a balanced chunk after the appending. This makes balancing have
at most n steps where n is the length of the right argument.

If we compare the number of balancing steps with left and right association,
we observe that, if the inputs are of similar size, it is better to associate
to the left.

        n                  n                     n
(--------------- <.> ---------------) <.> ---------------
=> n balancing steps
              2n                          n
------------------------------ <.> ---------------
=> n balancing steps
                    3n
---------------------------------------------

Total balancing steps: 2n

        n                   n                   n
--------------- <.> (--------------- <.> ---------------)
=> n balancing steps
        n                        2n
--------------- <.> ------------------------------
=> 2n balancing steps
                    3n
---------------------------------------------

Total balancing steps: 3n

Priority 5 is just a provisional number (very arbitrary).

-}

infixl 5 <.>

-- | /Sequencing of sounds/. The sequencing operator, as the name says, sequences a couple of
--   sounds.
--   There are some restriction to use this function.
--
--   * Both arguments must share the same /sample rate/.
--
--   * Both arguments must share the same /number of channels/.
(<.>) :: Sound -> Sound -> Sound
s1@(S r l nc c) <.> s2@(S r' l' nc' c')
 | r  /= r'  = soundError [s1,s2] "<.>" $ "Can't sequence sounds with different sample rates. "
                                       ++ "Please, consider to change the sample rate of one of them."
 | nc /= nc' = soundError [s1,s2] "<.>" $ "Can't sequence two sounds with different number of channels. "
                                       ++ "Please, consider to change the number of channels in one of them."
 | otherwise = S r (l+l') nc $ c <> c'

{-# RULES
"sound/multiplyFunction"
   forall n r d p f. multiply n (fromFunction r d p f) = fromFunction r d p (multiplySample n . f)
  #-}

-- | Multiply a sound over different channels. It will be just repeated over the different channels
--   with the same amplitude (unlike 'divide'). The number of channels will be multiplied by the
--   given factor.
--
-- > multiply n (fromFunction r d p f) = fromFunction r d p (multiplySample n . f)
--
multiply :: Int -- ^ Number of channels factor.
         -> Sound -> Sound
{-# INLINE[1] multiply #-}
multiply n s = f 1
 where
  f k = if k == n then s else s <|> f (k+1)

-- | Similar to 'multiply', but also dividing the amplitude of the sound by the factor.
divide :: Int -- ^ Number of channels factor.
       -> Sound -> Sound
{-# INLINE divide #-}
divide n s = scale (recip $ fromIntegral n) $ multiply n s

-- | This function works like '<+>', but it allows you to choose at which time add the sound.
--   This way, @insertAt t s1 s2@ will add @s1@ to @s2@ starting at the second @t@.
addAt :: Time -> Sound -> Sound -> Sound
addAt t s1 s2 = addSilenceBeg t s1 <+> s2

{-# RULES
"sound/velocity" forall f g s. velocity f (velocity g s) = velocity (\t -> f t * g t) s
  #-}

-- | Time-dependent amplitude modifier.
--
-- > velocity f (velocity g s) = velocity (\t -> f t * g t) s
--
velocity :: (Time -> Double) -- ^ @0 <= v t <= 1@.
         -> Sound
         -> Sound
{-# INLINE[1] velocity #-}
velocity v s = mapSoundAt (\i -> mapSample $ \x -> v (f i) * x) s
 where
  r = rate s
  f = sampleTime r

-- | Scale a sound by a given factor.
--
-- > scale = velocity . const
scale :: Double -- ^ Scaling factor. @0 <= k <= 1@
      -> Sound  -- ^ Original sound.
      -> Sound  -- ^ Scaled sound.
{-# INLINE scale #-}
scale = velocity . const

-- | Similar to the /par operator/ ('<|>') but using a time-dependent panning function.
--
-- > parWithPan (const (-1)) s1 s2 =              s1         <|>                     s2
-- > parWithPan (const   0 ) s1 s2 = scale (1/2) (s1 <+> s2) <|> scale (1/2) (s1 <+> s2)
-- > parWithPan (const   1 ) s1 s2 =                     s2  <|>              s1
--
parWithPan :: (Time -> Double) -- ^ @-1 <= p t <= 1@.
           -> Sound
           -> Sound
           -> Sound
{-# INLINE parWithPan #-}
parWithPan p s1@(S r1 n1 c1 ss1) s2@(S r2 n2 c2 ss2)
 | r1 /= r2 = soundError [s1,s2] "parWithPan" $ "Can't par sounds with different sample rates. "
                                             ++ "Please, consider to change the sample rate of one of them."
 | c1 /= c2 = soundError [s1,s2] "parWithPan" $ "Can't par sounds with different number of channels. "
                                             ++ "Please, consider to change the number of channels in one of them."
 | otherwise = S r1 (max n1 n2) (c1*2) $ if n1 == n2 then zipChunkedAtSame f ss1 ss2
                                                     else zipChunkedAt     f ss1 ss2
  where
   f i sx sy = let t  = sampleTime r1 i
                   q1 = (1 - p t) / 2
                   q2 = (1 + p t) / 2
                   l  = zipSamples (\x y -> q1*x + q2*y) sx sy
                   r  = zipSamples (\x y -> q1*y + q2*x) sx sy
               in appendSamples l r

-- | Pan a sound from left (-1) to right (1) with a time-dependent function.
--
-- > pan (const (-1)) = left
-- > pan (const   0 ) = divide 2
-- > pan (const   1 ) = right
--
pan :: (Time -> Double) -- ^ @-1 <= p t <= 1@.
    -> Sound
    -> Sound
pan p s = parWithPan p s $ zeroSoundWith (rate s) $ duration s

-- | Move a sound completely to the left.
left :: Sound -> Sound
left s = s <|> mapSound (mapSample $ const 0) s

-- | Move a sound completely to the right.
right :: Sound -> Sound
right s = mapSound (mapSample $ const 0) s <|> s

{-# RULES
"sound/loop"    forall n m s. loop n (loop m s) = loop (n*m) s
"sound/mapLoop" forall f n s. mapSound f (loop n s) = loop n (mapSound f s)
  #-}

-- | Repeat a sound cyclically a given number of times.
--   It obeys the following rules:
--
-- > loop n (loop m s) = loop (n*m) s
-- > mapSound f (loop n s) = loop n (mapSound f s)
--
loop :: Int -> Sound -> Sound
loop n = foldr1 (<.>) . replicate n

{-# RULES
"sound/mapTrim" forall t0 t1 f s. trim t0 t1 (mapSound f s) = mapSound f (trim t0 t1 s)
  #-}

-- | Extract a continous segment of the sound.
--
-- > trim t0 t1 (mapSound f s) = mapSound f (trim t0 t1 s)
--
trim :: Time  -- ^ Start time
     -> Time  -- ^ End time
     -> Sound
     -> Sound
trim t0 t1 s = trimIndex n0 n1 s
  where
    r  = rate s
    n0 = timeSample r t0
    n1 = timeSample r t1

trimIndex :: Int -- ^ Start index
          -> Int -- ^ End index
          -> Sound
          -> Sound
trimIndex n0 n1 s@(S r n c ss)
  | n0 >= n = S r 0 c mempty
  | n1 >= n = trimIndex n0 (n-1) s
  | otherwise = S r (n1-n0) c $ trimChunked n0 n1 ss

-- | Reverse a sound. Note that the entire sound is required
--   to be kept in memory to perform the reversion, since the
--   /first/ sample of the reversed sound is the /last/ sample
--   of the original. But to get the last sample the evaluation
--   of all the previous samples is required.
backwards :: Sound -> Sound
backwards s = s { schunks = reverseChunked $ schunks s }

-- ECHOING

-- | Echo effect.
--
-- > echo 0 dec del s = s
--
echo :: Int    -- ^ Repetitions. How many times the sound is repeated.
     -> Double -- ^ Decay (@0 < decay < 1@). How fast the amplitude of the repetitions decays.
     -> Time   -- ^ Delay @0 < delay@. Time between repetitions.
     -> Sound  -- ^ Original sound.
     -> Sound  -- ^ Echoed sound.
echo 0 _   _   s = s
echo n dec del s = s { schunks = causaltr f e $ schunks s }
  where
    e = Seq.empty
    m = timeSample (rate s) del
    f past x =
       ( let past' = if Seq.length past >= n*m
                        then seqInit past
                        else past
         in  x Seq.<| past'
       , let xs = [ if k <= Seq.length past
                       then Just $ mapSample (*q) $ Seq.index past (k-1)
                       else Nothing
                  | i <- [1 .. n]
                  , let k = i*m
                  , let q = dec ^ i
                    ]
         in  foldr1 (zipSamples (+)) $ x : catMaybes xs
         )

seqInit :: Seq.Seq a -> Seq.Seq a
seqInit xs = case Seq.viewr xs of
  ys Seq.:> _ -> ys
  _ -> Seq.empty

{-
-- INTEGRATION (possibly useful in the future)

simpson :: Time -> Time -> (Time -> Double) -> Double
simpson a b f = (b-a) / 6 * (f a + 4 * f ((a+b)/2) + f b)

intervalWidth :: Time
intervalWidth = 0.1

integrate :: Time -> Time -> (Time -> Double) -> Double
integrate a b f = sum [ simpson i (i + intervalWidth) f | i <- [a , a + intervalWidth .. b - intervalWidth] ]
-}

-- Simpson integration error
--
-- 1/90 * (intervalWidth/2)^5 * abs (f''''(c))
--

---------------
-- COMMON WAVES

{- About the common waves definitions

Functions describing these common waves have been created using
usual definitions, but then algebraically transformed to use a
smaller number of operations.

-}

-- | Double of 'pi'.
pi2 :: Time
pi2 = 2*pi

timeFloor :: Time -> Time
timeFloor = fromIntegral . (floor :: Time -> Int) -- Don't use truncate!

decimals :: Time -> Time
decimals = snd . (properFraction :: Time -> (Int,Time))

-- | Like 'zeroSound', but allowing to choose a custom sample rate.
zeroSoundWith :: Int -> Time -> Sound
{-# INLINE zeroSoundWith #-}
zeroSoundWith r d = S r n 1 $ zeroChunks n 1
 where
  n = timeSample r d

-- | Creates a mono and constantly null sound.
--
-- <<http://i.imgur.com/BP5PFIY.png>>
zeroSound :: Time -> Sound
{-# INLINE zeroSound #-}
zeroSound = zeroSoundWith 44100

sineRaw :: Int    -- ^ Sample rate
        -> Time   -- ^ Duration (0~)
        -> Double -- ^ Amplitude (0~1)
        -> Time   -- ^ Frequency (Hz)
        -> Time   -- ^ Phase
        -> Sound
{-# INLINE sineRaw #-}
sineRaw r d a f p = fromFunction r d (Just $ 1/f) $
  let pi2f = pi2*f
  in  \t ->
        let s :: Time
            s = pi2f*t + p
        in  monoSample $ a * sin s

-- | Like 'sine', but allowing to choose a custom sample rate.
sineWith :: Int    -- ^ Sample rate
         -> Time   -- ^ Duration (0~)
         -> Double -- ^ Amplitude (0~1)
         -> Time   -- ^ Frequency (Hz)
         -> Time   -- ^ Phase
         -> Sound
{-# INLINE sineWith #-}
sineWith r d a f = sineRaw r d' a f
  where
    q = recip $ 2*f
    (n,rm) = properFraction (d/q) :: (Int,Double)
    d' = if rm < 0.001 then d else fromIntegral (n+1) * q

-- | Create a sine wave with the given duration, amplitude, frequency and phase (mono).
--
-- <<http://i.imgur.com/46ry4Oq.png>>
sine :: Time   -- ^ Duration (0~)
     -> Double -- ^ Amplitude (0~1)
     -> Time   -- ^ Frequency (Hz)
     -> Time   -- ^ Phase
     -> Sound
{-# INLINE sine #-}
sine = sineWith 44100

-- | Like 'sineV', but allowing to choose the sample rate.
sineVR :: Int    -- ^ Sample rate
       -> Time   -- ^ Duration (0~)
       -> Double -- ^ Amplitude (0~1)
       -> (Time -> Time) -- ^ Frequency (Hz)
       -> Time   -- ^ Phase
       -> Sound
{-# INLINE sineVR #-}
sineVR r d a f p = fromFunction r d Nothing $
  \t -> let s :: Time
            s = pi2*f t*t + p
        in  monoSample $ a * sin s

-- | A variation of 'sine' with frequency that changes over time.
--   If you are going to use a constant frequency, consider to use
--   'sine' for a better performance.
sineV :: Time   -- ^ Duration (0~)
      -> Double -- ^ Amplitude (0~1)
      -> (Time -> Time) -- ^ Frequency (Hz)
      -> Time   -- ^ Phase
      -> Sound
{-# INLINE sineV #-}
sineV = sineVR 44100

-- | Like 'sawtooth', but allowing to choose the sample rate.
sawtoothRaw :: Int    -- ^ Sample rate
            -> Time   -- ^ Duration (0~)
            -> Double -- ^ Amplitude (0~1)
            -> Time   -- ^ Frequency (Hz)
            -> Time   -- ^ Phase
            -> Sound
{-# INLINE sawtoothRaw #-}
sawtoothRaw r d a f p = fromFunction r d (Just $ 1/f) $ \t ->
 let s :: Time
     s = f*t + p
 in  monoSample $ a * (2 * decimals s - 1)

sawtoothWith :: Int    -- ^ Sample rate
             -> Time   -- ^ Duration (0~)
             -> Double -- ^ Amplitude (0~1)
             -> Time   -- ^ Frequency (Hz)
             -> Time   -- ^ Phase
             -> Sound
{-# INLINE sawtoothWith #-}
sawtoothWith r d a f = sawtoothRaw r d' a f
  where
    q  = recip f
    (n,rm) = properFraction (d/q) :: (Int,Double)
    d' = if rm < 0.001 then d else fromIntegral (n+1) * q

-- | Create a sawtooth wave with the given duration, amplitude, frequency and phase (mono).
--
-- <<http://i.imgur.com/uJVIpmv.png>>
sawtooth :: Time   -- ^ Duration (0~)
         -> Double -- ^ Amplitude (0~1)
         -> Time   -- ^ Frequency (Hz)
         -> Time   -- ^ Phase
         -> Sound
{-# INLINE sawtooth #-}
sawtooth = sawtoothWith 44100

-- | Like 'square', but allowing to choose the sample rate.
squareWith :: Int    -- ^ Sample rate
           -> Time   -- ^ Duration (0~)
           -> Double -- ^ Amplitude (0~1)
           -> Time   -- ^ Frequency (Hz)
           -> Time   -- ^ Phase
           -> Sound
{-# INLINE squareWith #-}
squareWith r d a f = squareRaw r d' a f
  where
    q  = recip f
    (n,rm) = properFraction (d/q) :: (Int,Double)
    d' = if rm < 0.001 then d else fromIntegral (n+1) * q

squareRaw :: Int    -- ^ Sample rate
          -> Time   -- ^ Duration (0~)
          -> Double -- ^ Amplitude (0~1)
          -> Time   -- ^ Frequency (Hz)
          -> Time   -- ^ Phase
          -> Sound
{-# INLINE squareRaw #-}
squareRaw r d a f p = fromFunction r d (Just $ 1/f) $ \t ->
 let s :: Time
     s = f*t + p
     h :: Time -> Double
     h x = signum $ 0.5 - x
 in  monoSample $ a * h (decimals s)

-- | Create a square wave with the given duration, amplitude, frequency and phase (mono).
--
-- <<http://i.imgur.com/GQUCVwT.png>>
square :: Time   -- ^ Duration (0~)
       -> Double -- ^ Amplitude (0~1)
       -> Time   -- ^ Frequency (Hz)
       -> Time   -- ^ Phase
       -> Sound
{-# INLINE square #-}
square = squareWith 44100

triangleRaw :: Int    -- ^ Sample rate
            -> Time   -- ^ Duration (0~)
            -> Double -- ^ Amplitude (0~1)
            -> Time   -- ^ Frequency (Hz)
            -> Time   -- ^ Phase
            -> Sound
{-# INLINE triangleRaw #-}
triangleRaw r d a f p = fromFunction r d (Just $ 1/f) $ \t ->
 let s :: Time
     s = f*t + p
 in  monoSample $ a * (1 - 4 * abs (timeFloor (s + 0.25) - s + 0.25))

-- | As in 'triangle', but allowing to choose the sample rate.
triangleWith :: Int    -- ^ Sample rate
             -> Time   -- ^ Duration (0~)
             -> Double -- ^ Amplitude (0~1)
             -> Time   -- ^ Frequency (Hz)
             -> Time   -- ^ Phase
             -> Sound
{-# INLINE triangleWith #-}
triangleWith r d a f = triangleRaw r d' a f
  where
    q  = recip f
    (n,rm) = properFraction (d/q) :: (Int,Double)
    d' = if rm < 0.001 then d else fromIntegral (n+1) * q

-- | Create a triange wave with the given duration, amplitude, frequency and phase (mono).
--
-- <<http://i.imgur.com/0RZ8gUh.png>>
triangle :: Time   -- ^ Duration (0~)
         -> Double -- ^ Amplitude (0~1)
         -> Time   -- ^ Frequency (Hz)
         -> Time   -- ^ Phase
         -> Sound
{-# INLINE triangle #-}
triangle = triangleWith 44100

-------------------
-- OTHER SYNTHS

-- | Specialized random generator.
randomRs :: (Double,Double) -> Seed -> [Double]
randomRs (x,y) = go
  where
   go g = let (r,g') = range_random (x,y) g
          in  r : go g'

-- | Like 'pnoise', but allowing to choose the sample rate.
pnoiseR :: Int    -- ^ Sample rate
        -> Time   -- ^ Duration (0~)
        -> Double -- ^ Amplitude (0~1)
        -> Time   -- ^ Frequency (Hz)
        -> Word32 -- ^ Random seed
        -> Sound
{-# INLINE pnoiseR #-}
pnoiseR r d a f sd = S r tn 1 cs
 where
  n = timeSample r $ recip f
  xs = take n $ fmap monoSample $ randomRs (-a,a) $ seed [sd]
  tn = timeSample r d
  cs = chunkedFromList tn $ cycle xs

-- | A randomly generated sound (mono) with frequency. Different seeds generate
--   different sounds.
pnoise :: Time   -- ^ Duration (0~)
       -> Double -- ^ Amplitude (0~1)
       -> Time   -- ^ Frequency (Hz)
       -> Word32 -- ^ Random seed
       -> Sound
{-# INLINE pnoise #-}
pnoise = pnoiseR 44100

-- | Like 'karplus', but allowing to choose a custom sample rate.
karplusR :: Int    -- ^ Sample rate
         -> Time   -- ^ Duration (0~)
         -> Double -- ^ Amplitude (0~1)
         -> Time   -- ^ Frequency (Hz)
         -> Double -- ^ Decay (0~1)
         -> Word32 -- ^ Random seed
         -> Sound
{-# INLINE karplusR #-}
karplusR r d a f dc = velocity (dc**) . pnoiseR r d a f

-- | String-like sound based on randomly generated signals (see 'pnoise').
karplus :: Time   -- ^ Duration (0~)
        -> Double -- ^ Amplitude (0~1)
        -> Time   -- ^ Frequency (Hz)
        -> Double -- ^ Decay (0~1)
        -> Word32 -- ^ Random seed
        -> Sound
{-# INLINE karplus #-}
karplus = karplusR 44100

-- | A randomly generated sound (mono) without frequency. Different seeds generate
--   different sounds. For long sounds, a similar effect can be obtained using 'pnoise'
--   with much better performance. While 'noise' create new random values for the entire
--   length of the sound, 'pnoise' only creates a small portion that is repeated until
--   reaching the specified duration. If the frequency given to 'pnoise' is low enough
--   (any frequency lower than the human range should work) it should create a very similar
--   sound effect than the one 'noise' does.
noise :: Time   -- ^ Duration (0~)
      -> Double -- ^ Amplitude (0~1)
      -> Word32 -- ^ Random seed
      -> Sound
{-# INLINE noise #-}
noise = noiseR 44100

-- | Like 'noise', but allowing to choose a custom sample rate.
noiseR :: Int    -- ^ Sample rate
       -> Time   -- ^ Duration (0~)
       -> Double -- ^ Amplitude (0~1)
       -> Word32 -- ^ Random seed
       -> Sound
{-# INLINE noiseR #-}
noiseR r d a sd = S r n 1 $ chunkedFromList n xs
  where
    n  = timeSample r d
    xs = fmap monoSample $ randomRs (-a,a) $ seed [sd]

-- Misc

-- | Build an affine function given two points of its graph. The first
--   coordinate of the two points must be different.
affineFunction :: (Double,Double) -> (Double,Double) -> Double -> Double
affineFunction (a,b) (c,d) x = m*x + n
  where
    m = (d-b)/(c-a)
    n = b - m*a

-- | Build a linear function given a point of its graph outside of the Y axis.
--
-- > linearFunction = affineFunction (0,0)
--
linearFunction :: (Double,Double) -> Double -> Double
linearFunction = affineFunction (0,0)

--------------------------------
-- Room for experiments

fourierSieve :: Double -> Sound -> Sound
fourierSieve e s = s { schunks = f (schunks s) }
  where
    f = devectorize . fmap sampleFromVector . unsplitVector -- Reconstruct chunked data
      . fmap (vectorMakeReal .  g . vectorMakeComplex)      -- Manipulate vector
      . splitVector . fmap sampleVector . vectorize         -- Deconstruct chunked data
    g = fourierInverse . h . fourierTransform
    h = A.map $ \c -> if C.magnitude c > e then c else 0
