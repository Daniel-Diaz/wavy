
{-# LANGUAGE FlexibleContexts #-}

-- | PCM WAVE encoding.
module Data.Sound.WAVE (
   -- * Types
   WAVE (..)
 , Descriptor (..)
 , Format (..)
 , WAVEData (..)
   -- * Encoder/Decoder
 , ByteString
   -- ** Encoder
 , encode
 , encodeFile
 , encodeBuilders
   -- ** Decoder
 , decode
 , decodeFile
   -- * Sound
 , fromSound
 , toSound
   -- * Re-exports
   -- | Re-export of the "Data.Word" module for convenience.
 , module Data.Word
   ) where

import Data.Monoid
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as B
-- import Control.Arrow
import Control.Applicative
import Control.Monad

-- Binary encoding/decoding interface.
import Data.Binary.Builder
import Data.Binary.Get
import Data.Binary.IEEE754

-- Sound interface.
import Data.Sound.Internal

-- Chunked interface.
import Data.Sound.Core.Chunked

-- | Lazy bytestrings are used for encoding/decoding.
type ByteString = B.ByteString

-- | A sound stored in the 'WAVE' format.
data WAVE = WAVE {
   descriptor :: Descriptor
 , format     :: Format
 , waveData   :: WAVEData
 }

-- | The descriptor of the encoding.
data Descriptor = D {
   descSize :: Word32
 }

-- | Description of the properties of the wave stored.
data Format = Format {
   formatSize    :: Word32
 , audioFormat   :: Word16
 , numChannels   :: Word16
 , sampleRate    :: Word32
 , byteRate      :: Word32
 , blockAlign    :: Word16
 , bitsPerSample :: Word16
 }

-- | The actual waveform data.
data WAVEData = WData {
   dataSize :: Word32
 , samples  :: Chunked
 }

-- ENCODING

-- | Encode a 'WAVE' into a file. It is done lazily so the file will be written
--   whenever any data is available.
encodeFile :: FilePath -> WAVE -> IO ()
encodeFile fp = B.writeFile fp . encode

-- | Encode a 'WAVE' into a 'ByteString'.
--
-- > encode = toLazyByteString . mconcat . encodeBuilders
encode :: WAVE -> ByteString
encode = toLazyByteString . mconcat . encodeBuilders

-- | Encode a 'WAVE' into a list of 'Builder's. The size of each builder
--   is bounded by a constant depending on 'chunkSize'.
encodeBuilders :: WAVE -> [Builder]
encodeBuilders w =
 writeD (descriptor w) : writeFormat (format w) : writeData (waveData w) (bitsPerSample $ format w)

writeD :: Descriptor -> Builder
writeD d =
    fromLazyByteString (B.pack [82,73,70,70])
 <> putWord32le (descSize d)
 <> fromLazyByteString (B.pack [87,65,86,69])

writeFormat :: Format -> Builder
writeFormat f =
    fromLazyByteString (B.pack [102,109,116,32])
 <> putWord32le (formatSize f)
 <> putWord16le (audioFormat f)
 <> putWord16le (numChannels f)
 <> putWord32le (sampleRate f)
 <> putWord32le (byteRate f)
 <> putWord16le (blockAlign f)
 <> putWord16le (bitsPerSample f)

writeData :: WAVEData -> Word16 -> [Builder]
writeData d bd =
    fromLazyByteString (B.pack [100,97,116,97])
  : putWord32le (dataSize d)
  : let f = case bd of
             8  -> singleton   . cast8
             16 -> putWord16le . cast16
             24 -> error "wavy: 24-bits per sample is still not supported."
             32 -> putWord32le . cast32
             _  -> error $ "wavy: Unsupported bits-per-sample: " ++ show bd ++ "."
    in  linkedFoldChunked (foldrSample (\x xs -> f x <> xs) mempty) (samples d)

-- DECODING

matchBytes :: [Word8] -> Get ()
matchBytes xs = do
 ys <- replicateM (length xs) getWord8
 unless (xs == ys) $ fail $ "Bytes don't match! " ++ show xs ++ " /= " ++ show ys

-- | Try to decode a file to a 'WAVE'. If a failure is reached while reading the
--   WAVE header, a 'String' describing the decoding error will be returned.
--   Otherwise, all the data until the first failure will be returned.
decodeFile :: FilePath -> IO (Either String WAVE)
decodeFile = fmap decode . B.readFile

-- | Decode a lazy 'ByteString' to a 'WAVE'. While reading the data, if invalid
--   or incomplete input is found, the decoding will stop returning the data until
--   that point. No error will be reported. This allows a completly lazy approach.
--   However, the WAVE head will be read strictly.
decode :: ByteString -> Either String WAVE
decode b =
 case runGetOrFail getDescriptor b of
  Left (_,_,err) -> Left err
  Right (r1,_,d) ->
   case runGetOrFail getFormat r1 of
    Left (_,_,err) -> Left err
    Right (r2,_,fmt) ->
     case runGetOrFail getDataInfo r2 of
      Left (_,_,err) -> Left err
      Right (r3,_,di) ->
       let bd = bitsPerSample fmt
           nc = numChannels fmt
           k = div (8*di) $ fromIntegral (bd*nc)
           (q,r) = quotRem k $ fromIntegral chunkSize
           cs = decodeChunks bd nc (fromIntegral q) (fromIntegral r) r3
       in  Right $ WAVE d fmt $ WData di cs

decodeChunks :: Word16 -- ^ Bits per sample.
             -> Word16 -- ^ Number of channels.
             -> Int    -- ^ Number of chunks.
             -> Int    -- ^ Length of remainder chunk.
             -> ByteString -> Chunked
decodeChunks bd nc q r lb = go q lb
 where
  go 0 b =
   case runGetOrFail (getChunk bd nc r) b of
    Right (_,_,a) ->  Chunk a (fromIntegral r) Empty
    _ -> Empty
  go n b =
   case runGetOrFail (getChunk bd nc chunkSize) b of
    Right (b',_,a) -> Chunk a chunkSize $ go (n-1) b'
    _ -> Empty

getDescriptor :: Get Descriptor
getDescriptor =
   D
   <$> (matchBytes [82,73,70,70]
    *> getWord32le)
   <* matchBytes [87,65,86,69]

getFormat :: Get Format
getFormat =
   Format
   <$> (matchBytes [102,109,116,32]
    *> getWord32le)
   <*> getWord16le
   <*> getWord16le
   <*> getWord32le
   <*> getWord32le
   <*> getWord16le
   <*> getWord16le

getChunk :: Word16 -- ^ Bits per sample.
         -> Word16 -- ^ Number of channels.
         -> Int    -- ^ Chunk size (@<= chunkSize@).
         -> Get Array
getChunk bd nc cs =
 let ncI :: Int
     ncI = fromIntegral nc
     getSample :: Get Sample
     getSample = case bd of
       8  -> sampleFromList . fmap decast8  <$> replicateM ncI getWord8
       16 -> sampleFromList . fmap decast16 <$> replicateM ncI getWord16le
       24 -> fail "24-bits per sample is not supported yet."
       32 -> sampleFromList . fmap decast32 <$> replicateM ncI getWord32le
       _  -> fail $ "Wavy: Unsupported bits-per-sample: " ++ show bd ++ "."
 in fmap chunkFromList $ replicateM cs getSample

getDataInfo :: Get Word32
getDataInfo = matchBytes [100,97,116,97] *> getWord32le

-- Sound transformation

halfW :: Int -> Double
halfW n = (2^n - 1) / 2

half8 :: Double
half8 = halfW 8

half16 :: Double
half16 = halfW 16

half32 :: Double
half32 = halfW 32

cast8 :: Double -> Word8
cast8 x = fromIntegral . (truncate :: Double -> Int) $ -- Why is this faster than truncating directly to the word type?
            (x+1) * half8

decast8 :: Word8 -> Double
decast8 x = fromIntegral x / half8 - 1

cast16 :: Double -> Word16
cast16 x = fromIntegral . (truncate :: Double -> Int) $ -- Why is this faster than truncating directly to the word type?
     if x < 0 then    x  * half16
              else (x+2) * half16

decast16 :: Word16 -> Double
decast16 x_ = if x < 0 then x / half16
                       else x / half16 - 2
 where
  x = fromIntegral x_

-- Too slow!
cast32 :: Double -> Word32
-- cast32 x = fromIntegral . (truncate :: Double -> Int32) $ half32 * x
cast32 = floatToWord . (realToFrac :: Double -> Float)

decast32 :: Word32 -> Double
decast32 x = (/half32) . fromIntegral . (fromIntegral :: Word32 -> Int32) $ x

-- | Create a 'WAVE' header for a sound. You can write this 'WAVE' directly
--   to a file using 'encodeFile' or simply 'encode'.
fromSound :: Word16 -- ^ Bits per sample (bit depth).
          -> Sound  -- ^ Sound to give the WAVE format.
          -> WAVE   -- ^ WAVE output.
fromSound bd s = WAVE
   { descriptor = D $ 36 + sampleSize
   , format = Format
      { -- Word32
        formatSize = 16
        -- Word16
      , audioFormat = 1
        -- Word16
      , numChannels = nch
        -- Word32
      , sampleRate = sr
        -- Word32
      , byteRate = sr * nch * div (fromIntegral bd) 8
        -- Word16
      , blockAlign = nch * div bd 8
        -- Word16
      , bitsPerSample = bd
      }
   , waveData = WData
      { -- Word32
        dataSize = sampleSize
        -- Chunked
      , samples = schunks s
      }
   }
 where
  sr :: Word32
  sr = fromIntegral $ rate s
  nch :: Num a => a
  nch = fromIntegral $ channels s
  sampleSize :: Word32
  sampleSize = fromIntegral (nSamples s) * nch * div (fromIntegral bd) 8

-- | Makes a 'Sound' from a 'WAVE' data. You can get a 'WAVE'
--   from a file using 'decodeFile' or simply 'decode'.
toSound :: WAVE -> Sound
toSound w = S srt (fromIntegral $ div (dataSize dat) $ fromIntegral $ nc * div bd 8) nc $ samples dat
 where
  fmt = format w
  dat = waveData w
  --
  bd = bitsPerSample fmt
  nc :: Num a => a
  nc = fromIntegral $ numChannels fmt
  srt = fromIntegral $ sampleRate fmt
