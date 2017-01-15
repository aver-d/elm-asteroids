module Asteroids where
import Vec2d (Vec)
import Vec2d as V
import Window
import Rand
import Debug
import Keyboard
import Set
import Util (allPairs, roundTo, uniqBy)


fl = toFloat

debug = False

type Input = { dt: Time
             , fire: Bool
             , thrust: Bool
             , turn: Float }

type Asteroid = { id: Int
                , pos: Vec
                , vel: Vec
                , radius: Float
                , points: [Vec]}

type Ship = { pos: Vec
            , vel: Vec
            , accel: Vec
            , damping: Float
            , maxSpeed: Float
            , turnSpeed: Float
            , thrustPower: Float
            , radius: Float
            , heading: Float
            , thrust: Bool
            , firePower: Float
            , fireRate: Float }

type Bullet = { id: Int
              , pos: Vec
              , vel: Vec
              , speed: Float
              , heading: Float
              , radius: Float
              , timeToLive: Float }

type Game = { state: State
            , ship: Ship
            , asteroids: [Asteroid]
            , bullets: [Bullet]
            , seed: Rand.Seed
            , nextId: Int }

data State = Start | Play | End


(spaceWidth, spaceHeight) = (300, 150)
(halfW, halfH) = (spaceWidth // 2, spaceHeight // 2)



screenWrapper (w, h) ({pos, radius} as entity) =
  let (x, y) = pos
      newX = if abs x > w + radius then negate x else x
      newY = if abs y > h + radius then negate y else y
  in
    { entity | pos <- (newX, newY) }

wrap = screenWrapper (fl halfW, fl halfH)





-- Asteroid
--newAsteroid : Int -> Float -> Float -> Float -> Rand.Seed -> (Asteroid, Rand.Seed)
newAsteroid id x y radiusEst seed =
  let
    ((vx::vy::_), seed2) = Rand.ints -30 30 2 seed
    (r, seed3) = Rand.int (floor(radiusEst-4)) (floor(radiusEst+4)) seed2
    radius = toFloat r
    (npoints, seed4) = Rand.int 6 10 seed3
    (nums, seed5)  = Rand.floats npoints seed4

    angles = map (\n -> n * 2 *pi) <| map (roundTo 2) <| sort nums
    points = map (\a -> (cos a*radius, sin a*radius)) angles
  in
    ({id = id, pos = (x,y), vel = (fl vx, fl vy), radius = radius, points = points},
      seed5)

updateAsteroid dt asteroid =
  asteroid |> movePos dt |> wrap


-- Ship
defaultShip : Ship
defaultShip = { pos = (0,0)
              , vel = (0,0)
              , accel = (0,0)
              , damping = 0.99
              , maxSpeed = 100
              , turnSpeed = 2
              , thrustPower = 300
              , radius = 20
              , heading = 0
              , thrust = False
              , firePower = 0.25
              , fireRate = 0.25 }

movePos dt ent =
  { ent | pos <- ent.pos `V.add` (dt `V.mul` ent.vel) }


shipUpdate {dt, fire, turn, thrust} ship  =
  ship |> shipThrust thrust
       |> shipTurn dt turn
       |> shipReload dt
       |> shipFire fire
       |> shipMove dt
       |> wrap

shipMove dt ship =
  let vel = ship.vel `V.add` (dt `V.mul` ship.accel)
              |> V.mul ship.damping
              |> V.limit ship.maxSpeed
      pos = ship.pos `V.add` (dt `V.mul` vel)

  in  { ship | pos <- pos
             , vel <- vel
             , accel <- (0,0) }

shipTurn dt turn ship =
  if | turn == 0 -> ship
     | otherwise -> {ship | heading <- ship.heading + (turn * ship.turnSpeed * dt) }

shipThrust thrust ship =
  if | not thrust -> { ship | thrust <- False }
     | otherwise ->
        let force = V.mul ship.thrustPower (cos ship.heading, sin ship.heading)
        in { ship | accel <- force
                  , thrust <- True }

shipReload dt ship =
  { ship | firePower <- min (ship.firePower + dt) ship.fireRate }

shipFire fire ship =
  if | fire && ship.firePower >= ship.fireRate -> { ship | firePower <- 0 }
     | otherwise -> ship

didFire ship =
  ship.firePower == 0


-- Bullet
defaultBullet =
  { id = 0
  , pos = (0,0)
  , vel = (0,0)
  , timeToLive = 1
  , speed = 200
  , radius = 4
  , heading = 0 }

newBullet id ship =
  let ((x, y), (vx, vy), h) = (ship.pos, ship.vel, ship.heading)

  in {defaultBullet | pos <- ( x + (cos h) * ship.radius
                             , y + (sin h) * ship.radius )
                    , vel <- ( vx + (cos h) * defaultBullet.speed
                             , vy + (sin h) * defaultBullet.speed )
                    , heading <- h
                    , id <- id }

updateBullet dt bullet =
  bullet |> sapLife dt |> movePos dt

sapLife dt ent =
  {ent | timeToLive <- ent.timeToLive - dt }

isAlive {timeToLive} = timeToLive > 0


collide a b =
  V.len (V.sub a.pos b.pos) < (a.radius + b.radius)




-- Render
formAsteroid : Asteroid -> Form
formAsteroid a =
  if | not debug -> polygon a.points |> (outlined <| solid black) |> move a.pos
     | otherwise ->
        group [ polygon a.points |> (outlined <| solid black)
              , circle a.radius |> outlined (solid darkGreen) ] |> move a.pos

formBullet b =
  rect 4 3 |> filled red |> rotate b.heading |> move b.pos

formShip {pos, radius, heading, thrust} =
  let (x,y) = pos
      r = radius
      p = path [(-r/2,r), (r,0), (-r/2,-r)]
      body = path p |> (traced (solid darkBrown))
      booster y = rect (r/2) (r/3)
                  |> filled ( if thrust then red else charcoal )
                  |> move (-r, y/4)
      forms = booster r :: booster -r :: body :: [] |> group
  in
    forms |> move pos |> rotate heading

render : (Int,Int) -> Game -> Element
render (w, h) game =

  let ship      = formShip game.ship
      asteroids = map formAsteroid game.asteroids
      bullets   = map formBullet game.bullets
      forms     = ship :: (asteroids ++ bullets)

  in collage spaceWidth spaceHeight forms
      |> color gray
      |> container w h topLeft



-- Game
defaultGame : Game
defaultGame = { state = Start
              , ship = defaultShip
              , asteroids = []
              , bullets = []
              , seed = Rand.newSeed 0
              , nextId = 1 }

newGame numAsteroids =
  let seed = Rand.newSeed 0
      -- todo: put this creation stuff in a function to allow creation at later stage
      (xs, seed2) = Rand.ints -halfW halfW numAsteroids seed
      (ys, seed3) = Rand.ints -halfH halfH numAsteroids seed2

      (asteroids, seed4) =
        foldl (\ (x, y, id) (list, seed) ->
          let (ast, seed') = newAsteroid id x y 32 seed
          in  (ast::list, seed'))
        ([], seed3) (zip3 (map fl xs) (map fl ys) [1..numAsteroids])
  in
    {defaultGame | asteroids <- asteroids
                 , seed <- seed4
                 , nextId <- numAsteroids + 1
                 , state <- Play }




notHit collided =
  filter (\e -> not <| any (\{id} -> e.id == id) collided)

updateBullets dt collided =
  notHit collided >> filter isAlive >> map (updateBullet dt)

updateAsteroids dt collided =
  notHit collided >> map (movePos dt >> wrap)

addBullet nextId ship bullets =
  if | didFire ship -> ((newBullet nextId ship)::bullets, nextId + 1)
     | otherwise    -> (bullets, nextId)

addAsteroids nextId seed hitAsteroids asteroids =
  case hitAsteroids of
    [] -> (asteroids, nextId, seed)
    (parent::_) ->
      if | parent.radius < 12 -> (asteroids, nextId, seed)
         | otherwise ->            --todo: if parent radius less than some size, ignore
            let (n, seed') = Rand.int 2 5 seed -- number of children
                {pos, radius} = parent
                (children, seed'') = asteroidList nextId (repeat n pos) (radius/2) seed'

            in (asteroids ++ children, nextId + n, seed'')


asteroidList startId coords radius seed =
  foldl
    (\((x, y), id) (list, seed') ->
      -- todo: randomize radius or pass list of radii with coords
      let (ast, seed'') = newAsteroid id x y radius seed'
      in  (ast::list, seed''))

    ([], seed)
    (zip coords [startId .. startId+(length coords)])



colliders ents1 ents2 =
  let
    collisions = filter (uncurry collide) <| allPairs ents1 ents2
    hitEnts1 = map fst collisions |> uniqBy .id
    hitEnts2 = map snd collisions |> uniqBy .id
  in (hitEnts1, hitEnts2)



updateGame : Input -> Game -> Game
updateGame ({dt} as input) ({nextId, seed} as game) =

  case game.state of
    Start -> newGame 4
    Play ->
      let
          shipHit = any (collide game.ship) game.asteroids
          ship = shipUpdate input game.ship

          (hitAsteroids, hitBullets) = colliders game.asteroids game.bullets

          bullets = updateBullets dt hitBullets game.bullets
          asteroids = updateAsteroids dt hitAsteroids game.asteroids

          (bullets', nextId') = addBullet nextId ship bullets
          (asteroids', nextId'', seed') = addAsteroids nextId seed hitAsteroids asteroids


      in {game | ship <- ship
               , asteroids <- asteroids'
               , bullets <- bullets'
               , nextId <- nextId''
               , seed <- seed'}



-- Inputs
delta = inSeconds <~ fps 60

inputAll = sampleOn delta (Input <~ delta
                    {- fire   -}  ~ Keyboard.space
                    {- thrust -}  ~ lift (.y >> (==) 1 ) Keyboard.arrows
                    {- turn   -}  ~ lift (.x >> negate >> toFloat) Keyboard.arrows)


gameState = foldp updateGame defaultGame inputAll


main =
  render <~ Window.dimensions ~ gameState

