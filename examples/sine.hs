
import Data.Sound
import Data.Sound.WAVE

-- | This simple example shows how to synth a sine wave.
main :: IO ()
main = encodeFile "sine.wav" $ fromSound 16 s -- The resulting wave has 16 bit-depth.
 where
  -- A sine wave of 5 seconds, amplitude 1, frequency 100Hz and 0 phase.
  s = sine 5 1 50 0
