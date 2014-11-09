module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand
import Debug

fl = toFloat

type Input = { dt:Time }

type Asteroid = {pos: Vec, vel: Vec, radius: Float, points: [Vec]}
type Game = {state:State, asteroids:[Asteroid]}
data State = Play



-- Entities

newAsteroid : Float -> Float -> Rand.Seed -> (Asteroid, Rand.Seed)
newAsteroid x y seed =
  let
    ((vx::vy::_), seed2) = Rand.ints -10 10 2 seed
    (r, seed3)           = Rand.int 10 20 seed2
    radius = fl r
    (npoints, seed4)     = Rand.int 5 8 seed3
    (nums, seed5)        = Rand.floats npoints seed4

    angles = map (\n -> n * 2*pi) nums |> sort
    points = map (\a -> (cos a*radius, sin a*radius)) angles
  in
    ({pos = (x,y), vel = (fl vx, fl vy), radius = radius, points = points}, seed5)


stepAsteroid : Float -> Asteroid -> Asteroid
stepAsteroid dt a =
  { a | pos <- V.add a.pos (V.mul a.vel dt)}




-- Render
formAsteroid : Asteroid -> Form
formAsteroid {pos, radius} = circle radius |> filled black |> move pos


render : (Int,Int) -> Game -> Element
render (w, h) ({state, asteroids} as game) =

  let forms = map formAsteroid asteroids

  in collage w h forms
      |> color gray
      |> container w h middle



-- Game
defaultGame : Game
defaultGame = {state = Play,
               asteroids = [fst <| newAsteroid 16 16 <| Rand.newSeed 0]}

stepGame: Input -> Game -> Game
stepGame input game =
  let _ = Debug.watch "game" game
      _ = Debug.watch "input" input
      asteroids = map (stepAsteroid input.dt) game.asteroids
  in {game | asteroids <- asteroids}

gameState = foldp stepGame defaultGame inputAll



-- Inputs
delta = inSeconds <~ fps 30
inputAll = sampleOn delta (Input <~ delta)


main =
  --asText <| circlePoints [0.25, 0.5, 0.75] 1
  render <~ Window.dimensions ~ gameState