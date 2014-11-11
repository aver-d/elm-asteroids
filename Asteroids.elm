module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand
import Debug
import Util (bigrams, roundTo)

fl = toFloat

type Input = { dt:Time }

type Asteroid = {pos: Vec, vel: Vec, radius: Float, points: [Vec]}
type Game = {state:State, asteroids:[Asteroid], seed: Rand.Seed}
data State = Play


(spaceWidth, spaceHeight) = (600, 300)
(halfW, halfH) = (spaceWidth // 2, spaceHeight // 2)
(quartW, quartH) = (spaceWidth // 4, spaceHeight // 4)




screenWrapper (w, h) ({pos, radius} as entity) =
  let (x, y) = pos
      newX = if abs x > w + radius then negate x else x
      newY = if abs y > h + radius then negate y else y
  in
    { entity | pos <- (newX, newY) }

wrap = screenWrapper (fl halfW / 2, fl halfH / 2)


fillGaps maxGap (x::xs) =
  x :: (concatMap (\(a, b) ->
         let diff = b - a
         in if | diff <= maxGap -> [b]
               | otherwise ->
                 let n = ceiling (diff/maxGap) |> toFloat
                     multi = diff / n
                 in map (\i -> a + i * multi) [1..n]) <| bigrams <| x::xs)



-- Entities
newAsteroid : Float -> Float -> Rand.Seed -> (Asteroid, Rand.Seed)
newAsteroid x y seed =
  let
    ((vx::vy::_), seed2) = Rand.ints -10 10 2 seed
    (r, seed3)           = Rand.int 10 20 seed2
    radius = fl r
    (npoints, seed4)     = Rand.int 5 10 seed3
    (nums, seed5)        = Rand.floats npoints seed4

    --noGaps = fillGaps -1 <| sort nums
    noGaps = map (roundTo 2) (sort nums)
    _ = Debug.log "nums" noGaps

    angles = map (\n -> n * 2 *pi) noGaps
    points = map (\a -> (cos a*radius, sin a*radius)) angles
  in
    ({pos = (x,y), vel = (fl vx, fl vy), radius = radius, points = points}, seed5)



--moveEntity : Float -> a -> a
moveEntity dt ent =
  wrap { ent | pos <- V.add ent.pos (V.mul ent.vel dt)}


-- Render
formAsteroid : Asteroid -> Form
formAsteroid {pos, points} =
  polygon points |> (outlined <| solid black) |> move pos



render : (Int,Int) -> Game -> Element
render (w, h) ({state, asteroids} as game) =

  let forms = map formAsteroid asteroids

  in collage halfW halfH forms
      |> color gray
      |> container w h topLeft



-- Game
defaultGame : Game
defaultGame = { state = Play
              , asteroids = []
              , seed = Rand.newSeed 0 }

initGame numAsteroids =
  let seed = Rand.newSeed 0
      (xs, seed2) = Rand.ints -quartW quartW numAsteroids seed
      (ys, seed3) = Rand.ints -quartH quartH numAsteroids seed2

      (asteroids, seed4) =
        foldl (\ (x, y) (list, seed) ->
          let (ast, seed') = newAsteroid x y seed
          in  (ast::list, seed'))
        ([], seed3) (zip (map fl xs) (map fl ys))
  in
    {defaultGame | asteroids <- asteroids
                 , seed <- seed4}




stepGame : Input -> Game -> Game
stepGame input game =
  let
      -- _ = Debug.watch "game" game
      asteroids = map (moveEntity input.dt) game.asteroids

  in {game | asteroids <- asteroids}




-- Inputs
delta = inSeconds <~ fps 30
inputAll = sampleOn delta (Input <~ delta)


gameState = foldp stepGame (initGame 3) inputAll

main =
  --asText <|
  render <~ Window.dimensions ~ gameState