
-- | Internal representation of sounds chunks of samples.
module Data.Sound.Container.Chunks (
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
    -- * Chunks
  , Chunks , chunk
    -- ** Chunk size
    -- $csize
  , chunkSize , chunkSizeInt
  , halfChunkSize
    -- * Functions
  , (!) , (|>)
  , mapChunks
  , zipChunks , zipChunksAt
  , zipChunksSame, zipChunksAtSame
  , chunkFromList , chunksFromList
  , joinChunks
  , trimChunks
    -- * Folds
  , foldChunks , linkedFoldChunks
    -- * Traversals
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
monoSample = Sample . U.singleton

appendSamples :: Sample -> Sample -> Sample
appendSamples (Sample v) (Sample w) = Sample $ v U.++ w

-- | Create a 'Sample' with a given number of channels.
multiSample :: Int -> Double -> Sample
multiSample n x = Sample $ U.replicate n x

sampleLength :: Sample -> Int
sampleLength (Sample v) = U.length v

sampleFromList :: [Double] -> Sample
sampleFromList = Sample . U.fromList

foldrSample :: (Double -> r -> r) -> r -> Sample -> r
foldrSample f e (Sample v) = U.foldr f e v

mapSample :: (Double -> Double) -> Sample -> Sample
mapSample f (Sample v) = Sample $ U.map f v

multiplySample :: Int -> Sample -> Sample
multiplySample n (Sample v) =
  let l = U.length v
  in  Sample $ U.generate (n * l) $ \i -> U.unsafeIndex v $ mod i l

zipSamples :: (Double -> Double -> Double) -> Sample -> Sample -> Sample
zipSamples f (Sample v) (Sample w) = Sample $ U.zipWith f v w

type Array = A.Vector Sample

-- | The type of sample chunks. Samples are grouped in /chunks/. Each chunk
--   has at most 'chunkSize' samples. This property should not be checked
--   by any function, but every function has to preserve it.
--   'Chunks' type should not be exported by any other module.
data Chunks =
   Empty
 | Chunk {-# UNPACK #-} !Array  -- Array of samples
         {-# UNPACK #-} !Word32 -- Length of the array
                         Chunks -- Tail of chunks

-- | Chunk constructor. This function is /unsafe/ in the sense that does
--   not check if the array has the given length, nor check if the length
--   is at most 'chunkSize'. Please, use it only if you really know
--   these conditions are met.
chunk :: Array  -- ^ Chunk array.
      -> Word32 -- ^ Size of the array.
      -> Chunks -- ^ Chunks that go after the given array.
      -> Chunks
{-# INLINE chunk #-}
chunk = Chunk

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
(!) :: Chunks -> Word32 -> Maybe Sample
(Chunk a l t) ! n = if n < l then a A.!? fromIntegral n
                             else t ! (n-l)
Empty ! _ = Nothing

-- | /O(n/\/'chunkSize'/)/. Add a sample at the end of the sample chunks.
(|>) :: Chunks -> Sample -> Chunks
Empty |> x = Chunk (A.singleton x) 1 Empty
(Chunk a l t) |> x = if l == chunkSize
                        then Chunk a l $ t |> x
                        else Chunk (A.snoc a x) (l+1) t

{-# RULES
"chunks/map" forall f g c. mapChunks f (mapChunks g c) = mapChunks (\i x -> g i (f i x)) c
  #-}

-- | /O(n)/. Map every element of the sample chunks, using a function that
--   depends on the sample index.
mapChunks :: (Word32 -> Sample -> Sample) -> Chunks -> Chunks
mapChunks f = go 0
 where
  go n (Chunk a l t) = Chunk (A.imap (f . (+n) . fromIntegral) a) l $ go (n+l) t
  go _ Empty = Empty

-- FOLDS --

foldArray :: Monoid m => (Sample -> m) -> Array -> m
foldArray f = A.foldr (\x xs -> let y = f x in y <> xs) mempty

-- | Fold all the samples from the sample chunks using a monoid.
foldChunks :: Monoid m => (Sample -> m) -> Chunks -> m
foldChunks f = go
  where
    go (Chunk a _ _) | deepseq a False = undefined
    go (Chunk a _ t) = foldArray f a <> go t
    go Empty = mempty

-- | Fold each chunk of the sample chunks using a monoid.
linkedFoldChunks :: Monoid m => (Sample -> m) -> Chunks -> [m]
linkedFoldChunks f = go
  where
    go (Chunk a _ _) | deepseq a False = undefined
    go (Chunk a _ t) = foldArray f a : go t
    go Empty = []

-- Balanced appending turns out to be much more efficient in practice.
-- Balanced chunks make other operations (like zipping) more efficient.

-- | /O(min n m)/. Zip over balanced chunks. A parameter of the current index is supplied.
zipChunksAt :: (Word32 -> Sample -> Sample -> Sample) -> Chunks -> Chunks -> Chunks
zipChunksAt f = go 0
  where
    go n (Chunk a l t) (Chunk a' l' t') =
         let (lmin,lmax,maxa) = if l <= l' then (l,l',a') else (l',l,a)
             x = A.generate (fromIntegral lmax) $
                 \i -> let wi = fromIntegral i
                       in  if wi < lmin
                              then f (n+wi) (A.unsafeIndex a i) (A.unsafeIndex a' i)
                              else A.unsafeIndex maxa i
         in  Chunk x lmax $ go (n+chunkSize) t t'
    go _ c Empty = c
    go _ _ c = c

-- | /O(min n m)/. Zip over balanced chunks.
--
-- > zipChunks = zipChunksAt . const
--
zipChunks :: (Sample -> Sample -> Sample) -> Chunks -> Chunks -> Chunks
{-# INLINE zipChunks #-}
zipChunks = zipChunksAt . const

-- | Same as 'zipChunksAt', but it assumes both input 'Chunks' have the same length.
zipChunksAtSame :: (Word32 -> Sample -> Sample -> Sample) -> Chunks -> Chunks -> Chunks
zipChunksAtSame f = go 0
  where
    go n (Chunk a l t) (Chunk a' _ t') = Chunk (A.izipWith (f . (+n) . fromIntegral) a a') l
      $ go (n+chunkSize) t t'
    go _ _ _ = Empty

-- | Same as 'zipChunks', but it assumes both input 'Chunks' have the same length.
zipChunksSame :: (Sample -> Sample -> Sample) -> Chunks -> Chunks -> Chunks
{-# INLINE zipChunksSame #-}
zipChunksSame = zipChunksAtSame . const

-- | /O(length of the right argument)/. Balanced appending.
appendChunks :: Chunks -> Chunks -> Chunks
appendChunks (Chunk a l Empty) c@(Chunk a' l' t)
 | l == chunkSize = Chunk a l c
 | l' > d = let dInt = fromIntegral d
                sd = A.take dInt a'
            in  Chunk (a <> sd) chunkSize $ appendChunks (chunk (A.drop dInt a') (l'-d) Empty) t
 | otherwise = appendChunks (Chunk (a <> a') (l+l') Empty) t
    where
     d = chunkSize - l
appendChunks (Chunk a l t) c =
  chunk a l $ appendChunks t c
appendChunks _ c = c

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
chunksFromList :: Word32 -> [Sample] -> Chunks
chunksFromList n ss = h (1,ss)
 where
  f (a,l)  = chunk (chunkFromList a) l
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

-- | Join two chunks without checking the 'Chunks' properties. Therefore,
--   @joinChunks c1 c2@ must be called only when one of the following
--   conditions hold:
--
--   * @c1@ is Empty.
-- 
--   * Every chunk in @c1@ has size 'chunkSize'.
--
joinChunks :: Chunks -> Chunks -> Chunks
joinChunks (Chunk a l t) c = Chunk a l $ joinChunks t c
joinChunks _ c = c

trimChunks :: Word32 -> Word32 -> Chunks -> Chunks
trimChunks n0 n1 (Chunk a l t)
  | n0 >= l = trimChunks (n0-l) (n1-l) t
  | n1 <  l = Chunk (A.slice (fromIntegral n0) (fromIntegral $ n1-n0) a) (n1-n0) Empty
           <> t
  | n0 == 0 = Chunk a l $ trimChunks 0 (n1-l) t
  | otherwise = Chunk (A.drop (fromIntegral n0) a) (l-n0) Empty
             <> trimChunks 0 (n1-l) t
trimChunks _ _ _ = Empty

-- | Causal traversal.
causaltr :: (a -> Sample -> (a,Sample)) -> a -> Chunks -> Chunks
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

instance Monoid Chunks where
 mempty = Empty
 mappend = appendChunks

instance NFData Sample where
 rnf (Sample v) = rnf v

instance NFData Chunks where
 rnf Empty = ()
 rnf (Chunk a _ t) = rnf a `seq` rnf t
