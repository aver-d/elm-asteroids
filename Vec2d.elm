module Vec2d where

type Vec = (Float, Float)

add : Vec -> Vec -> Vec
add (vx, vy) (ux, uy) = (vx + ux, vy + uy)

sub : Vec -> Vec -> Vec
sub (vx, vy) (ux, uy) = (vx - ux, vy - uy)

mul : Float -> Vec -> Vec
mul n (x, y) = (x * n, y * n)

div n (x, y) =
  -- todo: error on div by zero
  (x/n, y/n)

len (x, y) = sqrt (x*x + y*y)

normalize v = div (len v) v

limit maxLen v =
  let l = len v
  in if | l > maxLen -> mul maxLen (div l v)
        | otherwise -> v
