module Rand (float, floats, int, ints, newSeed, Seed) where

import Bitwise
import Bitwise (and, shiftLeft, shiftRight)
xor = Bitwise.xor

type Seed = {w:Int, x:Int, y:Int, z:Int}

defaultSeed = {w = 88675123, x = 123456789, y = 362436069, z = 521288629 }
maxUInt32 = 4294967295 -- 0xffffffff
maxFloat = toFloat (maxUInt32 // 2)


newSeed : Int -> Seed
newSeed n =
  {defaultSeed | w <- ((n+104537) * 88675123) % maxUInt32 }

rand : Seed -> (Int, Seed)
rand {w,x,y,z} =
  let
    t = (x `xor` (x `shiftLeft` 11)) `and` maxUInt32
    (x',y',z') = (y, z, w)
    w' = (w `xor` (w `shiftRight` 19) `xor` (t `xor` (t `shiftRight` 8))) `and` maxUInt32
  in
    (w', {w = w', x = x', y = y', z = z'})


float : Seed -> (Float, Seed)
float seed =
  let (randInt, newSeed) = rand seed
  in (toFloat randInt / maxFloat, newSeed)


floats : Int -> Seed -> ([Float], Seed)
floats total seed =
  foldl (\_ acc ->
      let (n, seed') = float (snd acc)
      in  (n :: (fst acc), seed'))
    ([], seed)
    [0..total-1]

int : Int -> Int -> Seed -> (Int, Seed)
int lo hi seed =
  let (lo', hi') = (toFloat lo, toFloat hi)
      (n, seed') = float seed
  in (floor <| n * (hi' - lo') + lo', seed')


ints : Int -> Int -> Int -> Seed -> ([Int], Seed)
ints lo hi total seed =
  let (lo', hi') = (toFloat lo, toFloat hi)
      diff = hi' - lo'
  in foldl (\_ acc ->
      let (n, seed') = float (snd acc)
      in  ((floor <| n * diff + lo') :: (fst acc), seed'))
    ([], seed)
    [0..total-1]









