
import Criterion.Main
--
import Data.Sound
import Data.Sound.WAVE hiding (ByteString)

test :: Sound
test = sine 3 1 100 0

main :: IO ()
main = defaultMain [
    bgroup "Encoding"
     [ bench "8 bits per sample"  $ nf (\n -> encode $ fromSound n test) 8
     , bench "16 bits per sample" $ nf (\n -> encode $ fromSound n test) 16
     , bench "32 bits per sample" $ nf (\n -> encode $ fromSound n test) 32
       ]
  ]
