
import Data.Sound
import Data.Sound.WAVE

-- | This simple example shows how to synth a string using the Karplus-Strong algorithm.
main :: IO ()
main = encodeFile "karplus.wav" $ fromSound 16 s -- The resulting wave has 16 bit-depth.
 where
  -- A karplus-strong string of 2 seconds with amplitude 1, frequency 300Hz and decay of 0.01.
  -- The last argument (0 in this case) selects a seed to generate the random wave.
  s = karplus 2 1 300 0.01 0