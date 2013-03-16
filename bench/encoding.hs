
import Criterion.Main
--
import Data.Sound
import Data.Sound.WAVE hiding (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Lazy (ByteString,toChunks)
import Control.DeepSeq

instance NFData S.ByteString where

instance NFData ByteString where
 rnf = rnf . toChunks

test :: Sound
test = sine 3 1 100 0

main :: IO ()
main = defaultMain [
    bgroup "Encoding"
     [ bench "8 bits per sample"  $ nf (\n -> encode $ wave n test) 8
     , bench "16 bits per sample" $ nf (\n -> encode $ wave n test) 16
     , bench "32 bits per sample" $ nf (\n -> encode $ wave n test) 32
       ]
  ]