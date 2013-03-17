
import Data.Sound
import Data.Sound.WAVE

main :: IO ()
main = encodeFile "sineV.wav" $ fromSound 16 s
 where
  s = sineV 10 1 f 0
  q = (log 4000 - log 30)/10
  f t = 30 * exp (t*q)