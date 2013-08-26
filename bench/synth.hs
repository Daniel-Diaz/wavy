
import Criterion.Main
--
import Data.Sound
import Data.Sound.WAVE

main :: IO ()
main =
  defaultMain [
    bgroup "Basic wave"
     [ bench "sine"     $ nf (sine     3 1 100) 0
     , bench "sawtooth" $ nf (sawtooth 3 1 100) 0
     , bench "square"   $ nf (square   3 1 100) 0
     , bench "triangle" $ nf (triangle 3 1 100) 0
       ]
  , bgroup "Misc wave"
     [ bench "zero"     $ nf zeroSound 3
     , bench "noise"    $ nf (noise    3 1 100) 0
     , bench "karplus"  $ nf (karplus  3 1 100 0.01) 0
       ]
  ]
