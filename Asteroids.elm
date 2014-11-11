module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand
import Debug
import Keyboard
import Util (bigrams, roundTo)


fl = toFloat

debug = True

type Input = { dt: Time, fire: Bool, thrust: Bool, turn: Float }
type Asteroid = {pos: Vec, vel: Vec, radius: Float, points: [Vec]}
type Ship = { pos: Vec
            , vel: Vec
            , accel: Vec
            , damping: Float
            , maxspeed: Float
            , turnspeed: Float
            , thrustPower: Float
            , radius: Float
            , heading: Float
            , thrust: Bool
            , firePower: Float
            , fireRate: Float }
type Bullet = { pos: Vec
              , vel: Vec
              , timeToLive: Float
              , radius: Float }
type Game = { state: State
            , ship: Ship
            , asteroids: [Asteroid]
            , seed: Rand.Seed }
data State = Play | End


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
    ({pos = (x,y), vel = (fl vx, fl vy), radius = radius, points = points}, seed5)

newShip : Ship
newShip =
  { pos = (0,0), vel = (0,0), accel = (0,0), damping = 0.99, maxspeed = 100
  , turnspeed = 2, thrustPower = 300, radius = 20, heading = 0, thrust = False
  , firePower = 0.25, fireRate = 0.25 }

movePos dt ent =
  wrap { ent | pos <- ent.pos `V.add` (dt `V.mul` ent.vel) }


shipUpdate {dt, fire, turn, thrust} ship  =
  ship |> shipThrust thrust
       |> shipTurn dt turn
       |> shipFire dt fire
       |> shipMove dt
       |> wrap


shipMove dt ship =
  let vel = ship.vel `V.add` (dt `V.mul` ship.accel)
              |> V.mul ship.damping
              |> V.limit ship.maxspeed
      pos = ship.pos `V.add` (dt `V.mul` vel)

  in  { ship | pos <- pos
             , vel <- vel
             , accel <- (0,0) }

shipTurn dt turn ship =
  if | turn == 0 -> ship
     | otherwise -> {ship | heading <- ship.heading + (turn * ship.turnspeed * dt) }


shipThrust thrust ship =
  if | not thrust -> ship
     | otherwise ->
        let force = V.mul ship.thrustPower (cos ship.heading, sin ship.heading)
        in { ship | accel <- force }

shipFire dt fire ship =
  if | fire      -> { ship | firePower <- 0 }
     | otherwise -> { ship | firePower <- min (ship.firePower + dt) ship.fireRate    }

hasFired ship =
  ship.firePower == 0


defaultBullet =
  { pos = (0,0)
  , vel = (0,0)
  , timeToLive = 1
  , speed = 200
  , radius = 4 }

newBullet ship =
  let ((x, y), (vx, vy), h) = (ship.pos, ship.vel, ship.heading)

  in {defaultBullet | pos <- ( x + (cos h) * ship.radius
                             , y + (sin h) * ship.radius )
                    , vel <- ( vx + (cos h) * defaultBullet.speed
                             , vy + (sin h) * defaultBullet.speed ) }



collide p q =
  V.len (V.sub p.pos q.pos) < (p.radius + q.radius)

-- Render
formAsteroid : Asteroid -> Form
formAsteroid a =
  if | not debug -> polygon a.points |> (outlined <| solid black) |> move a.pos
     | otherwise ->
        group [ polygon a.points |> (outlined <| solid black)
              , circle a.radius |> outlined (solid darkGreen) ] |> move a.pos



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





updateGame : Input -> Game -> Game
updateGame input game =
  let -- _ = if input.fire then Debug.log "fire" input.fire else False
      --_ = Debug.log "dt" input.dt
      shipHit = any (collide game.ship) game.asteroids
      ship = shipUpdate input game.ship
      _ = if hasFired ship then  Debug.log "fired!" True else False
      asteroids = map (movePos input.dt) game.asteroids

      _ = Debug.watch "ship" ship

  in {game | asteroids <- asteroids
           , ship <- ship }



-- Inputs
delta = inSeconds <~ fps 60
inputAll = sampleOn delta (Input <~ delta
                      {- fire -}  ~ Keyboard.space
                    {- thrust -}  ~ lift (.y >> (==) 1 ) Keyboard.arrows
                      {- turn -}  ~ lift (.x >> negate >> toFloat) Keyboard.arrows)


gameState = foldp updateGame (newGame 5) inputAll


main =

  render <~ Window.dimensions ~ gameState