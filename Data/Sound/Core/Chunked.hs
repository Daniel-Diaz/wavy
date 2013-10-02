
-- | Internal representation of sounds chunks of samples.
module Data.Sound.Core.Chunked (
    -- * Samples
    Sample
  , monoSample
  , multiSample
  , sampleLength
  , sampleFromList
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
  , chunkSize , chunkSizeInt
  , halfChunkSize
    -- ** Vectors
  , vectorize
    -- ** Functions
  , (!) , (|>)
  , mapChunked
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

import Data.Word
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
newtype Sample = Sample (U.Vector Double)

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

foldrSample :: (Double -> r -> r) -> r -> Sample -> r
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
         {-# UNPACK #-} !Word32  -- Length of the array
                         Chunked -- Tail of chunks

-- | Chunk size should be even, so @chunkSize = 2 * halfChunkSize@.
--   Current value is @22050@.
chunkSize :: Word32
chunkSize = 22050

-- | A variant of 'chunkSize' of type 'Int'.
chunkSizeInt :: Int
chunkSizeInt = fromIntegral chunkSize

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
halfChunkSize :: Word32
halfChunkSize = div chunkSize 2

infixr 2 !

-- | /O(n/\/'chunkSize'/)/. Extract an element from the sample chunks.
--   Index starts at zero.
(!) :: Chunked -> Word32 -> Maybe Sample
(Chunk a l t) ! n = if n < l then a A.!? fromIntegral n
                             else t ! (n-l)
_ ! _ = Nothing

-- | /O(n/\/'chunkSize'/)/. Add a sample at the end of the sample chunks.
(|>) :: Chunked -> Sample -> Chunked
(Chunk a l t) |> x = if l == chunkSize
                        then Chunk a l $ t |> x
                        else Chunk (A.snoc a x) (l+1) t
_ |> x = Chunk (A.singleton x) 1 Empty

-- | Collapse a chunked sequence of samples into a single 'A.Vector'.
vectorize :: Chunked -> A.Vector Sample
vectorize (Chunk a _ Empty) = a
vectorize (Chunk a _ t)     = a <> vectorize t
vectorize Empty = mempty

{-# RULES
"chunks/map" forall f g c. mapChunked f (mapChunked g c) = mapChunked (\i x -> g i (f i x)) c
  #-}

-- | /O(n)/. Map every element of the sample chunks, using a function that
--   depends on the sample index.
mapChunked :: (Word32 -> Sample -> Sample) -> Chunked -> Chunked
mapChunked f = go 0
 where
  go n (Chunk a l t) = Chunk (A.imap (f . (+n) . fromIntegral) a) l $ go (n+l) t
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
zipChunkedAt :: (Word32 -> Sample -> Sample -> Sample) -> Chunked -> Chunked -> Chunked
zipChunkedAt f = go 0
  where
    go n (Chunk a l t) (Chunk a' l' t') =
         let (lmin,lmax,maxa) = if l <= l' then (l,l',a') else (l',l,a)
             x = A.generate (fromIntegral lmax) $
                 \i -> let wi = fromIntegral i
                       in  if wi < lmin
                              then f (n+wi) (A.unsafeIndex a i) (A.unsafeIndex a' i)
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
zipChunkedAtSame :: (Word32 -> Sample -> Sample -> Sample) -> Chunked -> Chunked -> Chunked
{-# INLINE zipChunkedAtSame #-}
zipChunkedAtSame f = go 0
  where
    go n (Chunk a l t) (Chunk a' _ t') = Chunk (A.izipWith (f . (+n) . fromIntegral) a a') l
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
 | l' > d = let dInt = fromIntegral d
                sd = A.take dInt a'
            in  Chunk (a <> sd) chunkSize $ appendChunked (Chunk (A.drop dInt a') (l'-d) Empty) t
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
chunkedFromList :: Word32 -> [Sample] -> Chunked
chunkedFromList n ss = h (1,ss)
 where
  f (a,l)  = Chunk (chunkFromList a) l
  g (i,xs) = if i > q then if i == q + 1 then Just ((take (fromIntegral r) xs,r),(i+1,[]))
                                         else Nothing
                      else let (ys,zs) = splitAt chunkSizeI xs
                           in  Just ((ys,chunkSize),(i+1,zs))
  h z = case g z of
          Just (x,y) -> f x (h y)
          _ -> Empty
  chunkSizeI :: Int
  chunkSizeI = fromIntegral chunkSize
  (q,r) = quotRem n chunkSize

trimChunked :: Word32 -> Word32 -> Chunked -> Chunked
trimChunked n0 n1 (Chunk a l t)
  | n0 >= l = trimChunked (n0-l) (n1-l) t
  | n1 <  l = Chunk (A.slice (fromIntegral n0) (fromIntegral $ n1-n0) a) (n1-n0) Empty
           <> t
  | n0 == 0 = Chunk a l $ trimChunked 0 (n1-l) t
  | otherwise = Chunk (A.drop (fromIntegral n0) a) (l-n0) Empty
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
