
import Data.Sound
import Data.Sound.WAVE

-- | This simple example shows how to synth a square wave.
main :: IO ()
main = encodeFile "square.wav" $ fromSound 16 s -- The resulting wave has 16 bit-depth.
 where
  -- A square wave of 5 seconds, amplitude 1, frequency 100Hz and 0 phase.
  s = square 5 1 100 0