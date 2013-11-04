
-- | Internal representation of sounds chunks of samples.
module Data.Sound.Core.Chunked (
    -- * Samples
    Sample
  , sampleVector
  , monoSample
  , multiSample
  , sampleLength
  , sampleFromList
  , sampleFromVector
    -- ** Sample functions
  , appendSamples
  , mapSample
  , foldrSample
  , multiplySample
  , zipSamples
    -- * Chunked
  , Array, Chunked (..)
    -- ** Chunk size
    -- $csize
  , chunkSize
  , halfChunkSize
    -- ** Vectors
  , vectorize
  , devectorize
    -- ** Functions
  , (!) , (|>)
  , mapChunked
  , mapChunkedArrays
  , zipChunked , zipChunkedAt
  , zipChunkedSame, zipChunkedAtSame
  , chunkFromList , chunkedFromList
  , trimChunked, reverseChunked
    -- ** Folds
  , foldrChunked
  , foldChunked , linkedFoldChunked
    -- ** Traversals
  , causaltr
  ) where

import qualified Data.Vector as A
import qualified Data.Vector.Unboxed as U
--
import Data.Monoid
import Data.Traversable (mapAccumL)
import Control.DeepSeq

--------------------------------------
---------- Public interface ----------
--------------------------------------

-- | A sample is a list of numbers from -1 to 1, one number per channel.
newtype Sample = Sample { sampleVector :: U.Vector Double }

monoSample :: Double -> Sample
{-# INLINE monoSample #-}
monoSample = Sample . U.singleton

appendSamples :: Sample -> Sample -> Sample
appendSamples (Sample v) (Sample w) = Sample $ v U.++ w

-- | Create a 'Sample' with a given number of channels.
multiSample :: Int -> Double -> Sample
{-# INLINE multiSample #-}
multiSample n x = Sample $ U.replicate n x

sampleLength :: Sample -> Int
sampleLength (Sample v) = U.length v

sampleFromList :: [Double] -> Sample
sampleFromList = Sample . U.fromList

sampleFromVector :: U.Vector Double -> Sample
sampleFromVector = Sample

foldrSample :: (Double -> r -> r) -> r -> Sample -> r
{-# INLINE foldrSample #-}
foldrSample f e (Sample v) = U.foldr f e v

mapSample :: (Double -> Double) -> Sample -> Sample
{-# INLINE mapSample #-}
mapSample f (Sample v) = Sample $ U.map f v

multiplySample :: Int -> Sample -> Sample
multiplySample n (Sample v) =
  let l = U.length v
  in  Sample $ U.generate (n * l) $ \i -> U.unsafeIndex v $ mod i l

zipSamples :: (Double -> Double -> Double) -> Sample -> Sample -> Sample
{-# INLINE zipSamples #-}
zipSamples f (Sample v) (Sample w) = Sample $ U.zipWith f v w

type Array = A.Vector Sample

-- | A chunked sequence of samples. A valid 'Chunked' value is determined
--   by the following laws.
--
-- * A value of type 'Chunked' is either 'Empty' or 'Chunk'@ a l t@, where
--   @a@ is an 'Array' of 'Sample's, @l@ is a 'Word32' value with the /length/ of @a@
--   and @t@ is a /valid/ 'Chunked' value (the /tail/).
--
-- * If @c == @'Chunk'@ a l t@ is a 'Chunked' value, then @l <= @'chunkSize'.
--   If @l < @'chunkSize', then @t == @'Empty'.
--
-- The evaluation of a 'Chunked' sequence is done by chunks. If a non-empty
-- 'Chunked' sequence is reduced to WHNF, then the first chunk of data is
-- evaluated, while a pointer to the /tail/ is kept lazy.
data Chunked =
   Empty
 | Chunk {-# UNPACK #-} !Array   -- Array of samples
         {-# UNPACK #-} !Int     -- Length of the array
                         Chunked -- Tail of chunks

-- | Chunk size is chosen as a power of two for efficiency reasons.
--   Current value is @2^15 = 32768@.
chunkSize :: Int
chunkSize = 2^(15::Int)

-- * About the Chunk Size

{- $csize

The performance of the library depends directly on 'chunkSize'.
A big value of 'chunkSize' will make appending faster, but will also increase memory usage.
The current value is 44100/2(=22050), which means that a sound with sample rate of 44,100Hz will be broken in
chunks of half second. However, this is just a provisional value.

-}

-- | > halfChunkSize = div chunkSize 2
--
--  Currently unused.
halfChunkSize :: Int
halfChunkSize = div chunkSize 2

infixr 2 !

-- | /O(n/\/'chunkSize'/)/. Extract an element from the sample chunks.
--   Index starts at zero.
(!) :: Chunked -> Int -> Maybe Sample
(Chunk a l t) ! n = if n < l then a A.!? n
                             else t ! (n-l)
_ ! _ = Nothing

-- | /O(n/\/'chunkSize'/)/. Add a sample at the end of the sample chunks.
(|>) :: Chunked -> Sample -> Chunked
(Chunk a l t) |> x = if l == chunkSize
                        then Chunk a l $ t |> x
                        else Chunk (A.snoc a x) (l+1) t
_ |> x = Chunk (A.singleton x) 1 Empty

getChunks :: Chunked -> [Array]
getChunks (Chunk a _ t) = a : getChunks t
getChunks Empty = []

-- | Collapse a 'Chunked' sequence of samples into a single 'Array'.
vectorize :: Chunked -> Array
vectorize = A.concat . getChunks

-- | Break an 'Array' of samples in a 'Chunked' sequence.
devectorize :: Array -> Chunked
devectorize v = chunkedFromList (A.length v) $ A.toList v

-- | Map a 'Chunked' array of samples, chunk by chunk.
--   The transformation function /must/ keep the array size
--   of the input. Otherwise, the output 'Chunked' value will
--   be malformed.
mapChunkedArrays :: (Array -> Array) -> Chunked -> Chunked
mapChunkedArrays f = go
  where
    go (Chunk a l t) = Chunk (f a) l $ go t
    go _ = Empty

{-# RULES
"chunks/map" forall f g c. mapChunked f (mapChunked g c) = mapChunked (\i x -> g i (f i x)) c
  #-}

-- | /O(n)/. Map every element of the sample chunks, using a function that
--   depends on the sample index.
mapChunked :: (Int -> Sample -> Sample) -> Chunked -> Chunked
mapChunked f = go 0
 where
  go n (Chunk a l t) = Chunk (A.imap (f . (+n)) a) l $ go (n+l) t
  go _ _ = Empty

-- FOLDS --

foldArray :: Monoid m => (Sample -> m) -> Array -> m
foldArray f = A.foldr (\x xs -> let y = f x in y <> xs) mempty

-- | Right-fold of the samples.
foldrChunked :: (Sample -> a -> a) -> a -> Chunked -> a
foldrChunked f e = go
  where
    go (Chunk a _ t) = A.foldr f (go t) a
    go _ = e

-- | Fold all the samples from the sample chunks using a monoid.
foldChunked :: Monoid m => (Sample -> m) -> Chunked -> m
foldChunked f = go
  where
    go (Chunk a _ t) = foldArray f a <> go t
    go _ = mempty

-- | Fold each chunk of the sample chunks using a monoid.
linkedFoldChunked :: Monoid m => (Sample -> m) -> Chunked -> [m]
linkedFoldChunked f = go
  where
    go (Chunk a _ t) = foldArray f a : go t
    go _ = []

-- Balanced appending turns out to be much more efficient in practice.
-- Balanced chunks make other operations (like zipping) more efficient.

-- | /O(min n m)/. Zip over balanced chunks. A parameter of the current index is supplied.
zipChunkedAt :: (Int -> Sample -> Sample -> Sample) -> Chunked -> Chunked -> Chunked
zipChunkedAt f = go 0
  where
    go n (Chunk a l t) (Chunk a' l' t') =
         let (lmin,lmax,maxa) = if l <= l' then (l,l',a') else (l',l,a)
             x = A.generate lmax $
                 \i -> if i < lmin
                          then f (n+i) (A.unsafeIndex a i) (A.unsafeIndex a' i)
                          else A.unsafeIndex maxa i
         in  Chunk x lmax $ go (n+lmax) t t'
    go _ c Empty = c
    go _ _ c = c

-- | /O(min n m)/. Zip over balanced chunks.
--
-- > zipChunked = zipChunkedAt . const
--
zipChunked :: (Sample -> Sample -> Sample) -> Chunked -> Chunked -> Chunked
{-# INLINE zipChunked #-}
zipChunked = zipChunkedAt . const

-- | Same as 'zipChunkedAt', but it assumes both input 'Chunked' have the same length.
zipChunkedAtSame :: (Int -> Sample -> Sample -> Sample) -> Chunked -> Chunked -> Chunked
zipChunkedAtSame f = go 0
  where
    go n (Chunk a l t) (Chunk a' _ t') = Chunk (A.izipWith (f . (+n)) a a') l
      $ go (n+chunkSize) t t'
    go _ _ _ = Empty

-- | Same as 'zipChunked', but it assumes both input 'Chunked' have the same length.
zipChunkedSame :: (Sample -> Sample -> Sample) -> Chunked -> Chunked -> Chunked
{-# INLINE zipChunkedSame #-}
zipChunkedSame = zipChunkedAtSame . const

-- | /O(length of the right argument)/. Balanced appending.
appendChunked :: Chunked -> Chunked -> Chunked
appendChunked (Chunk a l Empty) c@(Chunk a' l' t)
 | l == chunkSize = Chunk a l c
 | l' > d = let sd = A.take d a'
            in  Chunk (a <> sd) chunkSize $ appendChunked (Chunk (A.drop d a') (l'-d) Empty) t
 | otherwise = appendChunked (Chunk (a <> a') (l+l') Empty) t
    where
     d = chunkSize - l
appendChunked (Chunk a l t) c =
  Chunk a l $ appendChunked t c
appendChunked _ c = c

-- | /O(n)/. Create a sample array from a list of samples.
chunkFromList :: [Sample] -> Array
{-# INLINE chunkFromList #-}
chunkFromList = A.fromList

-- Hylomorphism fusion
--
-- foldr f e . unfoldr g = h
--   where
--    h z = case g z of
--            Just (x,y) -> f x (h y)
--            Nothing -> e

-- | /O(n)/. Create sample chunks from a list, grouping samples in arrays
--   of 'chunkSize'.
chunkedFromList :: Int -> [Sample] -> Chunked
chunkedFromList n ss = h (1,ss)
 where
  f (a,l)  = Chunk (chunkFromList a) l
  g (i,xs) = if i > q then if i == q + 1 then Just ((take r xs,r),(i+1,[]))
                                         else Nothing
                      else let (ys,zs) = splitAt chunkSize xs
                           in  Just ((ys,chunkSize),(i+1,zs))
  h z = case g z of
          Just (x,y) -> f x (h y)
          _ -> Empty
  (q,r) = quotRem n chunkSize

trimChunked :: Int -> Int -> Chunked -> Chunked
trimChunked n0 n1 (Chunk a l t)
  | n0 >= l = trimChunked (n0-l) (n1-l) t
  | n1 <  l = Chunk (A.slice n0 (n1-n0) a) (n1-n0) Empty
           <> t
  | n0 == 0 = Chunk a l $ trimChunked 0 (n1-l) t
  | otherwise = Chunk (A.drop n0 a) (l-n0) Empty
             <> trimChunked 0 (n1-l) t
trimChunked _ _ _ = Empty

reverseChunked :: Chunked -> Chunked
reverseChunked (Chunk a l t) = reverseChunked t <> Chunk (A.reverse a) l Empty
reverseChunked _ = Empty

-- | Causal traversal.
causaltr :: (a -> Sample -> (a,Sample)) -> a -> Chunked -> Chunked
causaltr f = go
  where
    go e (Chunk a l t) =
       let (e',a') = mapAccumL f e a
       in  Chunk a' l $ go e' t
    go _ _ = Empty

--------------------------------------
--------------------------------------
--------------------------------------

--------------------------------------
------------- INSTANCES --------------
--------------------------------------

instance Monoid Chunked where
 mempty = Empty
 mappend = appendChunked

instance NFData Sample where
 rnf (Sample v) = rnf v

instance NFData Chunked where
 rnf Empty = ()
 rnf (Chunk a _ t) = rnf a `seq` rnf t
