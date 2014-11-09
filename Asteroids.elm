module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand

fl = toFloat

type Input = { delta:Time }

type Asteroid = {pos: Vec, vel: Vec, radius: Float, points: [Vec]}
type Game = {state:State, asteroids:[Asteroid]}
data State = Play



-- Inputs
delta = inSeconds <~ fps 30
inputAll = sampleOn delta (Input <~ delta)

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


-- Render
formAsteroid : Asteroid -> Form
formAsteroid {pos, radius} = circle radius |> filled black |> move pos


render : (Int,Int) -> Game -> Element
render (w, h) ({state, asteroids} as game) =

  let forms = map formAsteroid asteroids
      formsPlus = (asText game |> toForm) :: forms

  in collage w h formsPlus
      |> color gray
      |> container w h middle



-- Game
defaultGame : Game
defaultGame = {state = Play,
               asteroids = []}

stepGame: Input -> Game -> Game
stepGame input game = game

gameState = foldp stepGame defaultGame inputAll


main =
  --asText <| circlePoints [0.25, 0.5, 0.75] 1
  lift2 render Window.dimensions gameState