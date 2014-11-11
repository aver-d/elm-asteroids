module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand
import Debug
import Keyboard
import Util (bigrams, roundTo)


fl = toFloat

type Input = { dt:Time, thrust: Bool, turn: Float }


type Asteroid = {pos: Vec, vel: Vec, accel: Vec, radius: Float, points: [Vec]}
type Ship = { pos: Vec
            , vel: Vec
            , accel: Vec
            , damping: Float
            , maxspeed: Float
            , turnspeed: Float
            , radius: Float
            , heading: Float
            , thrust: Bool }
type Game = { state: State
            , ship: Ship
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

wrap = screenWrapper (fl halfW, fl halfH)





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

newShip : Ship
newShip =
  { pos = (0,0), vel = (0,0), accel = (0,0), damping = 0.98, maxspeed = 10
  , turnspeed = 2, radius = 20, heading = 0, thrust = False }

movePos dt ent =
  wrap { ent | pos <- ent.pos `V.add` (dt `V.mul` ent.vel) }


stepShip {dt, turn, thrust} ship  =
  ship shipThrust thrust |> shipTurn dt turn |> shipMove dt


shipMove dt ship =
  let vel = ship.vel `V.add` (dt `V.mul` ship.accel)
              |> V.mul ship.damping
              |> V.limit ship.maxspeed
      pos = ship.pos `V.add` (dt `V.mul` vel)

  in  wrap { ship | pos <- pos
                  , vel <- vel
                  , accel <- (0,0) }

shipTurn dt turn ship =
  let heading = if | turn == 0 -> ship.heading
                   | otherwise -> ship.heading + (turn * ship.turnspeed * dt)
  in
    { ship | heading <- heading }


shipThrust thrust ship =
  if | thrust ->
        let force = (cos ship.heading, sin ship.heading) |> V.mul ship.thrustPower
        in {ship | accel <- force }
     | otherwise -> ship

--shipFire




-- Render
formAsteroid : Asteroid -> Form
formAsteroid {pos, points} =
  polygon points |> (outlined <| solid black) |> move pos


formShip {pos, radius, heading, thrust} =
  let (x,y) = pos
      r = radius
      p = path [(-r/2,r), (r,0), (-r/2,-r)]
      body = path p |> (traced (solid darkBrown))
      booster y = rect (r/2) (r/3) |> filled red |> move (-r, y/4)
      forms = booster r :: booster -r :: body :: [] |> group
  in
    forms |> move pos |> rotate heading




render : (Int,Int) -> Game -> Element
render (w, h) game =

  let ship = formShip game.ship
      asteroids = map formAsteroid game.asteroids
      forms = ship :: asteroids

  in collage spaceWidth spaceHeight forms
      |> color gray
      |> container w h topLeft



-- Game
defaultGame : Game
defaultGame = { state = Play
              , ship = newShip
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
       --_ = if not <| turn == 0 then let _ = Debug.log "turn" turn in 1 else 1
      ship = stepShip input game.ship
      asteroids = map (movePos input.dt) game.asteroids

  in {game | asteroids <- asteroids
           , ship <- ship }



-- Inputs
delta = inSeconds <~ fps 30
inputAll = sampleOn delta (Input <~ delta
                                  ~ lift (.y >> (==) 1 ) Keyboard.arrows  --thrust
                                  ~ lift (.x >> negate >> toFloat) Keyboard.arrows) --turn


gameState = foldp stepGame (newGame 10) inputAll


main =

  render <~ Window.dimensions ~ gameState