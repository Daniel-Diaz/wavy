
-- | Internal representation of sounds chunks of samples.
module Data.Sound.Container.Chunks (
    -- * Types
    Sample , Chunks , chunk
    -- * Chunk size
    -- $csize
  , chunkSize , chunkSizeInt
  , halfChunkSize
    -- * Functions
  , (!) , (|>)
  , mapChunks
  , zipChunks , zipChunksAt
  , chunkFromList , chunksFromList
  , joinChunks
    -- * Folds
  , foldChunks , linkedFoldChunks
  ) where

import Data.Word
import qualified Data.Vector as A
--
import Data.Monoid
import Control.DeepSeq

--------------------------------------
---------- Public interface ----------
--------------------------------------

-- | A sample is a list of numbers from -1 to 1, one number per channel.
type Sample = [Double]

-- Do not export this type. It is only for 'Chunks' use.
type Array = A.Vector Sample

-- | The type of sample chunks. Samples are grouped in /chunks/. Each chunk
--   has at most 'chunkSize' samples. This property should not be checked
--   by any function, but every function has to preserve it.
--   'Chunks' type should not be exported by any other module.
data Chunks =
   Empty
 | Chunk {-# UNPACK #-} !Array
         {-# UNPACK #-} !Word32
                         Chunks

-- | Chunk constructor. This function is /unsafe/ in the sense that does
--   not check if the array has the given length, nor check if the length
--   is at most 'chunkSize'. Please, use it only if you really know
--   these conditions are met.
chunk :: Array  -- ^ Chunk array.
      -> Word32 -- ^ Size of the array.
      -> Chunks -- ^ Chunks that go after the given array.
      -> Chunks
chunk a l t = Chunk a l t

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
(!) :: Chunks -> Word32 -> Maybe Sample
c ! 0 = c ! 1
(Chunk a l t) ! n = if n > l
                       then t ! (n-l)
                       else Just $ a A.! (fromIntegral n - 1)
Empty ! _ = Nothing

-- | /O(n/\/'chunkSize'/)/. Add a sample at the end of the sample chunks.
(|>) :: Chunks -> Sample -> Chunks
Empty |> x = Chunk (A.singleton x) 1 Empty
(Chunk a l t) |> x = if l == chunkSize
                        then chunk a l $ t |> x
                        else chunk (A.snoc a x) (l+1) t

{-# RULES
"chunks/map" forall f g c. mapChunks f (mapChunks g c) = mapChunks (\i x -> g i (f i x)) c
  #-}

-- | /O(n)/. Map every element of the sample chunks, using a function that
--   depends on the sample index.
mapChunks :: (Word32 -> Sample -> Sample) -> Chunks -> Chunks
mapChunks _ Empty = Empty
mapChunks f (Chunk a l t) =
  chunk (A.imap (f . fromIntegral) a) l $ mapChunks (\i x -> f (i+l) x) t

-- FOLDS --

foldArray :: Monoid m => (Sample -> m) -> Array -> m
foldArray f a = A.foldr (\x xs -> let y = f x in y <> xs) mempty a

-- | Fold all the samples from the sample chunks using a monoid.
foldChunks :: Monoid m => (Sample -> m) -> Chunks -> m
foldChunks f = go
  where
    go Empty = mempty
    go (Chunk a _ _) | deepseq a False = undefined
    go (Chunk a _ t) = foldArray f a <> go t

-- | Fold each chunk of the sample chunks using a monoid.
linkedFoldChunks :: Monoid m => (Sample -> m) -> Chunks -> [m]
linkedFoldChunks f = go
  where
    go Empty = []
    go (Chunk a _ _) | deepseq a False = undefined
    go (Chunk a _ t) = foldArray f a : go t

-- Balanced appending turns out to be much more efficient in practice.
-- Balanced chunks make other operations (like zipping) more efficient.

-- | /O(min n m)/. Zip over balanced chunks. A parameter of the current index is supplied.
zipChunksAt :: (Word32 -> Sample -> Sample -> Sample) -> Chunks -> Chunks -> Chunks
zipChunksAt f (Chunk a l t) (Chunk a' l' t') =
  if l > l' then chunk (A.izipWith (f . fromIntegral) a a' <> A.drop (fromIntegral l') a ) l  $
                   zipChunksAt f t t'
            else chunk (A.izipWith (f . fromIntegral) a a' <> A.drop (fromIntegral l ) a') l' $
                   zipChunksAt f t t'
zipChunksAt _ c Empty = c
zipChunksAt _ Empty c = c

-- | /O(min n m)/. Zip over balanced chunks.
--
-- > zipChunks = zipChunksAt . const
--
zipChunks :: (Sample -> Sample -> Sample) -> Chunks -> Chunks -> Chunks
zipChunks = zipChunksAt . const

-- | /O(length of the right argument)/. Balanced appending.
appendChunks :: Chunks -> Chunks -> Chunks
appendChunks Empty c = c
appendChunks c Empty = c
appendChunks (Chunk a l Empty) c@(Chunk a' l' t)
 | l == chunkSize = chunk a l c
 | l' > d = let sd = A.take (fromIntegral d) a'
            in  chunk (a <> sd) chunkSize $ appendChunks (chunk (A.drop (fromIntegral d) a') (l'-d) Empty) t
 | otherwise = appendChunks (chunk (a <> a') (l+l') Empty) t
    where
     d = chunkSize - l
appendChunks (Chunk a l t) c =
  chunk a l $ appendChunks t c

-- | /O(n)/. Create a sample array from a list of samples.
chunkFromList :: [Sample] -> Array
chunkFromList = A.fromList

-- Hylomorphism fusion
--
-- foldr f e . unfoldr g = h
--   where
--    h z = case g z of
--            Nothing -> e
--            Just (x,y) -> f x (h y)
--
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
          Nothing -> Empty
          Just (x,y) -> f x (h y)
  chunkSizeI :: Int
  chunkSizeI = fromIntegral chunkSize
  (q,r) = quotRem n chunkSize

-- | Join two chunks without checking the 'Chunks' properties. Therefore,
--   @joinChunks c1 c2@ must be called only when one of the following
--   conditions hold:
--
--   * @c1@ is Empty.
-- 
--   * Every chunk in @c1@ excluding the last one has size 'chunkSize'.
--
joinChunks :: Chunks -> Chunks -> Chunks
joinChunks (Chunk a l t) c = Chunk a l $ joinChunks t c
joinChunks Empty c = c

--------------------------------------
--------------------------------------
--------------------------------------

--------------------------------------
------------- INSTANCES --------------
--------------------------------------

instance Monoid Chunks where
 mempty = Empty
 mappend = appendChunks

instance NFData Chunks where
 rnf Empty = ()
 rnf (Chunk a _ t) = rnf a `seq` rnf t