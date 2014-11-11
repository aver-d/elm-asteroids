module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand
import Debug
import Util (bigrams, roundTo)

fl = toFloat

type Input = { dt:Time }

type Asteroid = {pos: Vec, vel: Vec, accel: Vec, radius: Float, points: [Vec]}
type Spaceship = {pos: Vec, vel: Vec, accel: Vec, radius: Float, heading: Float}
type Game = { state: State
            , spaceship: Spaceship
            , asteroids: [Asteroid]
            , seed: Rand.Seed }
data State = Play


(spaceWidth, spaceHeight) = (300, 150)
(halfW, halfH) = (spaceWidth // 2, spaceHeight // 2)



screenWrapper (w, h) ({pos, radius} as entity) =
  let (x, y) = pos
      newX = if abs x > w + radius then negate x else x
      newY = if abs y > h + radius then negate y else y
  in
    { entity | pos <- (newX, newY) }

wrap = screenWrapper (fl halfW , fl halfH)





-- Entities
newAsteroid : Float -> Float -> Rand.Seed -> (Asteroid, Rand.Seed)
newAsteroid x y seed =
  let
    ((vx::vy::_), seed2) = Rand.ints -10 10 2 seed
    (r, seed3)           = Rand.int 10 20 seed2
    radius = fl r
    (npoints, seed4)     = Rand.int 5 10 seed3
    (nums, seed5)        = Rand.floats npoints seed4

    angles = map (\n -> n * 2 *pi) <| map (roundTo 2) <| sort nums
    points = map (\a -> (cos a*radius, sin a*radius)) angles
  in
    ({pos = (x,y), vel = (fl vx, fl vy), accel=(0,0), radius = radius, points = points}, seed5)


newSpaceship =
  {pos = (0,0), vel = (0,0), accel = (0,0), radius = 8, heading = 0}

--moveEntity : Float -> a -> a
moveEntity dt ent =
  let vel = V.add ent.vel <| V.mul ent.accel dt
      pos = V.add ent.pos <| V.mul vel dt
  in  wrap { ent | pos <- pos
                 , vel <- vel }


-- Render
formAsteroid : Asteroid -> Form
formAsteroid {pos, points} =
  polygon points |> (outlined <| solid black) |> move pos


formSpaceship {pos, radius, heading} =
  let (x,y) = pos
      r = radius
      p = path [(-r/2,r), (r,0), (-r/2,-r)]
  in
    path p |> (traced (solid black)) |> move pos |> rotate heading


render : (Int,Int) -> Game -> Element
render (w, h) game =

  let forms = formSpaceship game.spaceship :: (map formAsteroid game.asteroids)

  in collage spaceWidth spaceHeight forms
      |> color gray
      |> container w h topLeft



-- Game
defaultGame : Game
defaultGame = { state = Play
              , spaceship = newSpaceship
              , asteroids = []
              , seed = Rand.newSeed 0 }

newGame numAsteroids =
  let seed = Rand.newSeed 0
      (xs, seed2) = Rand.ints -halfW halfW numAsteroids seed
      (ys, seed3) = Rand.ints -halfH halfH numAsteroids seed2

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


gameState = foldp stepGame (newGame 3) inputAll

main =
  --asText <|
  render <~ Window.dimensions ~ gameState