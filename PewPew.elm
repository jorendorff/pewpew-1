import Html exposing (Html, div)
import Html.App as Html
import Html.Events exposing (..)
import AnimationFrame
import Keyboard exposing (KeyCode)
import Json.Decode as Json
import Svg exposing (..)
import Svg.Attributes
import Svg.Attributes exposing (..)
import Time exposing (Time, second, inSeconds, millisecond)
import List
import List exposing (..)
import String

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

(gameWidth, gameHeight) = (640, 480)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

type State = Play | Win | Lose

type alias Object a = { a | x : Float, y : Float, vx : Float, vy : Float}

type alias Ship = Object {}

type alias Enemy = Object {
    lastFired : Time
}

type alias Explosion = Object {
    time : Time
}

type alias Projectile = Object {}

type alias Buttons =
  { left: Bool
  , right: Bool
  , fire: Bool
  }

type alias Model =
  { score : Int
  , duration : Time
  , state : State
  , ship : Ship
  , projectiles : List Projectile
  , enemies : List Enemy
  , explosions : List Explosion
  , enemyProjectiles : List Projectile
  , lastUpdateTime : Maybe Time
  , lastPlayerFireTime : Time
  , buttons : Buttons
  }

enemyStartSpeed = 25.0
enemyEndSpeed = 200.0

cubicEasing : (Int, Float) -> (Int, Float) -> Int -> Float
cubicEasing (x0, y0) (x1, y1) x =
    let nx = -1.0 + 2.0 * toFloat (x - x0) / toFloat (x1 - x0)
        ymid = (y0 + y1) / 2.0
    in ymid + (y1 - ymid) * nx^3

enemyVelocity : Int -> Int -> Int -> Float
enemyVelocity dir enemiesRemaining enemiesInitial =
    let speed = cubicEasing (enemiesInitial, enemyStartSpeed) (1, enemyEndSpeed) enemiesRemaining
    in toFloat dir * speed

makeEnemy : (Int, Int) -> Enemy
makeEnemy (row, col) =
   let enemySize = 30
       y = (toFloat row * enemySize) + 20.0
       x = (toFloat col * enemySize)
       vx = enemyStartSpeed
   in { x=x, y=y, vx=vx, vy=0.0, lastFired=0 }

starCoordinates : (Int, Int) -> List Char -> List (Int, Int)
starCoordinates (row, col) chars = case chars of
  [] -> []
  ('\n'::rest) -> starCoordinates (row + 1, 0) rest
  ('*'::rest) -> (row, col) :: starCoordinates (row, col + 1) rest
  (_::rest) -> starCoordinates (row, col + 1) rest

asciiToEnemies : String -> List Enemy
asciiToEnemies string =
    let positions = String.toList string |> starCoordinates (0, 0)
    in map makeEnemy positions

level : List Enemy
level = asciiToEnemies """
 * *    **   * *
 ****  *  *  ****
   *    **     *
"""


-- """
--         ***     *******         ***
--        ****    ***   ***       ****
--       *****   ***     ***     *****
--      ** ***   ***     ***    ** ***
--     *** ***   ***     ***   *** ***
--    ***  ***   ***     ***  ***  ***
--   *********** ***    **** ***********
--         ***    ***   ***        ***
--        *****     *****         *****
-- """

fleetSize = length level

init : (Model, Cmd Msg)
init =
  ( { score            = 0
    , duration         = 0
    , state            = Play
    , ship             = { x = halfWidth - 100, y = gameHeight - 20, vx = 0, vy=0 }
    , projectiles      = []
    , enemies          = level
    , explosions       = []
    , enemyProjectiles = []
    , lastUpdateTime     = Nothing
    , lastPlayerFireTime = -1.0
    , buttons            =
        { right = False
        , left = False
        , fire = False
        }
    }
  , Cmd.none
  )


-- UPDATE

tickDuration   = 100 * millisecond
reloadDuration = 350 * millisecond

-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c

-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
stepObj : Object a -> Object a
stepObj ({x,y,vx,vy} as obj) =
    { obj | x = x + vx * Time.inSeconds tickDuration
          , y = y + vy * Time.inSeconds tickDuration }

isOnScreen : Object a -> Bool
isOnScreen {x,y} =
    y < gameHeight
    && y > 0
    && x < gameWidth
    && x > 0

within : Float -> Float -> Object a -> Object b -> Bool
within x y a b =
     (a.x |> near b.x x) && (a.y |> near b.y y)

except : List a -> List a -> List a
except a b =
    let inB x = any ((==) x) b
    in List.filter (not << inB) a

stepShip : Int -> Ship -> Ship
stepShip dir ship =
    let shipWidth = 40
        vx' = toFloat dir * 400
        ship' = stepObj { ship | vx = vx' }
        x' = clamp (shipWidth/2) (gameWidth - shipWidth/2) ship'.x
    in
      { ship' | x = x' }


stepProjectiles : Bool -> Float -> List Projectile -> List Projectile
stepProjectiles firing origin projectiles =
    let projectiles' = projectiles
        |> List.map stepObj
        |> List.filter isOnScreen

    in case firing of
        True -> { x = origin, y = gameHeight - 20, vx = 0, vy=-400 } :: projectiles'
        _ -> projectiles'


stepEnemies : List Enemy -> List Enemy
stepEnemies enemies =
    case enemies of
        [] -> []
        (first::rest) ->
            let enemies' = enemies
                            |> List.map stepObj
                            |> List.map (\e -> {e | lastFired = e.lastFired + tickDuration})
                count = length enemies'
                positions = List.map .x enemies'
            in case (minimum positions, maximum positions) of
                 (Just low, Just high) ->
                   let (onEdge, dir) =
                         if low <= 30 then (True, 1)
                         else if high > gameWidth - 30 then (True, -1)
                              else (False, if first.vx > 0 then 1 else -1)
                   in List.map (\enemy -> { enemy | y = if onEdge then enemy.y + 8 else enemy.y,
                                                    vx = enemyVelocity dir count fleetSize }) enemies'
                 (_, _) -> []


shouldFire : Int -> Enemy -> Int -> Bool
shouldFire enemiesRemaining enemy index =
    let interval = (cubicEasing (fleetSize, 10.0) (1, 1.0) enemiesRemaining) * second
        wobble   = abs(tan(toFloat index)) * (toFloat enemiesRemaining) * (0.5 * second)

    in enemy.lastFired > interval + wobble


tryEnemyFire : Int -> (Int,Enemy) -> (Enemy, Maybe Projectile)
tryEnemyFire enemiesRemaing (index,enemy) =
    case shouldFire enemiesRemaing enemy index  of
    True  -> ({enemy | lastFired = 0},
              Just {
                  x = enemy.x,
                  y = enemy.y,
                  vy = 10,
                  vx = enemy.vx })
    False -> (enemy,Nothing)

-- Acceleration of gravity (determined experimentally)
gravity = 158.0

-- Horizontal wind braking factor
braking = 0.3

accelerateProjectile : Projectile -> Projectile
accelerateProjectile p = {p | vx = p.vx * (braking ^ Time.inSeconds tickDuration)
                            , vy = p.vy + gravity * Time.inSeconds tickDuration}

withIndex list = map2 (,) [0..length list] list

stepEnemyFire : List Projectile -> List Enemy -> (List Enemy,List Projectile)
stepEnemyFire projectiles enemies =
    let indexed = enemies |> withIndex
        projectiles' = projectiles
                        |> List.map stepObj
                        |> List.filter isOnScreen
                        |> List.map accelerateProjectile

        (enemies',newProjectiles) = indexed
                                    |> List.map (tryEnemyFire (length enemies))
                                    |> unzip
        enemies'' = enemies' |> List.map (\enemy -> {enemy | lastFired = enemy.lastFired + tickDuration})
        projectiles'' = (newProjectiles |> (List.filterMap identity)) ++ projectiles'

    in (enemies'', projectiles'')


stepEnemyCollisions : List Projectile -> List Enemy -> (List Projectile,List Enemy,List Explosion)
stepEnemyCollisions projectiles enemies =
    let hits = projectiles |> concatMap ((flip List.map enemies) << (,)) |> List.filter (uncurry (within 14 8))
        (hitProjectiles, hitEnemies) = unzip hits
        explosions = hitEnemies
            |> List.map (\enemy -> {
                time = inSeconds 150 * millisecond,
                vx = enemy.vx / 1.2 ,
                vy = 0,
                x = enemy.x,
                y = enemy.y
            })
    in
        (projectiles `except` hitProjectiles, enemies `except` hitEnemies, explosions)


stepExplosions : List Explosion -> List Explosion
stepExplosions explosions =
    let burn e = {e | time = e.time - tickDuration}
    in explosions
        |> List.map stepObj
        |> List.map burn
        |> List.filter ((<) 0 << .time)


pointsScored : Time -> Int -> Int
pointsScored duration enemiesHit =
    enemiesHit * Basics.max (truncate (300 - duration)) 1

stepPlay : Model -> Model
stepPlay ({buttons, score, duration, ship, projectiles, enemies, explosions, enemyProjectiles} as game) =
    let
        direction = (if buttons.left then -1 else 0) + (if buttons.right then 1 else 0)
        ship' : Ship
        ship' = stepShip direction ship
        reallyFiring : Bool
        reallyFiring = buttons.fire && duration > game.lastPlayerFireTime + reloadDuration
        projectiles' : List Projectile
        projectiles' = stepProjectiles reallyFiring ship.x projectiles
        (enemies', enemyProjectiles') = case enemies of
            [] -> (enemies, enemyProjectiles)
            _  -> stepEnemies enemies |> (stepEnemyFire enemyProjectiles)
        (projectiles'', enemies'', explosions') = stepEnemyCollisions projectiles' enemies'
        explosions'' = explosions' ++ stepExplosions explosions
        duration' = duration + tickDuration
        score' : Int
        score' = score + pointsScored duration' (length enemies - length enemies'')
        state' : State
        state' = case enemies of
            [] -> Win
            _  -> if enemyProjectiles' |> any (within 12 10 ship') then Lose
                  else if enemies' |> any (within 12 10 ship') then Lose
                       else Play

    in
        {game |
            score = score',
            duration = duration',
            ship = ship',
            projectiles = projectiles'',
            enemies = enemies'',
            explosions = explosions'',
            state = state',
            enemyProjectiles = enemyProjectiles',
            lastPlayerFireTime = if reallyFiring then duration' else game.lastPlayerFireTime,
            lastUpdateTime = Just (Maybe.withDefault 0 game.lastUpdateTime + tickDuration)
        }

advance : Time -> Model -> Model
advance t game =
    let last = Maybe.withDefault t game.lastUpdateTime
        game' = {game | lastUpdateTime = Just last }
        dt = t - last
    in foldl (\_ g -> stepPlay g) game' [1 .. floor (dt / tickDuration)]

rightKey = 39
leftKey = 37
fireKey = 32 -- space

updateButton b k value =
  if k == leftKey then { b | left = value }
  else if k == rightKey then { b | right = value }
  else if k == fireKey then { b | fire = value }
  else b

updateButtons b msg = case msg of
  KeyDown k -> updateButton b k True
  KeyUp k -> updateButton b k False
  _ -> b

type Msg =
    KeyDown KeyCode
  | KeyUp KeyCode
  | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown k -> ({ model | buttons = updateButtons model.buttons msg }, Cmd.none)
    KeyUp k -> ({ model | buttons = updateButtons model.buttons msg }, Cmd.none)
    Tick newTime -> (advance newTime model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Play -> Sub.batch
              [ AnimationFrame.times Tick
              , Keyboard.downs KeyDown
              , Keyboard.ups KeyUp ]
    _  -> Sub.none


-- VIEW

displayProjectile : Projectile -> Svg Msg
displayProjectile proj =
    rect [ x (toString proj.x)
         , y (toString proj.y)
         , width "2px"
         , height "6px"
         , fill "white"
         ]
         []

sprite url (inWidth, inHeight) outWidth transforms =
  image [ xlinkHref url
        , x "0"
        , y "0"
        , width (toString inWidth ++ "px")
        , height (toString inHeight ++ "px")
        , transform ("scale(" ++ toString (outWidth / inWidth) ++ ")" ++ transforms)
        ]
        []

blob color (inWidth, inHeight) outWidth transforms =
  rect [ fill color
       , x (toString (0 - inWidth / 2) ++ "px")
       , y (toString (0 - inHeight / 2) ++ "px")
       , width (toString inWidth ++ "px")
       , height (toString inHeight ++ "px")
       , transform ("scale(" ++ toString (outWidth / inWidth) ++ ")" ++ transforms)
       ]
       []

shipSprite : Svg Msg
shipSprite = blob "blue" (288, 266) 40 ""

explosionSprite : Svg Msg
explosionSprite = blob "yellow" (326, 301) 30 ""

enemySprite : Svg Msg
enemySprite = blob "red" (288, 266) 30 " rotate(180)"

translate : {a | x : Float, y : Float} -> Svg Msg -> Svg Msg
translate {x, y} svg =
  g [ transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")") ] [ svg ]

displayShip : State -> Ship -> Svg Msg
displayShip state ship =
  translate ship (case state of
                    Lose -> explosionSprite
                    _    -> shipSprite)

displayEnemy : Enemy -> Svg Msg
displayEnemy enemy = translate enemy enemySprite

displayExplosion : Explosion -> Svg Msg
displayExplosion boom =
  translate boom (g [ transform ("scale(" ++ toString (1.2 * boom.time/0.15) ++ ")") ]
                    [ explosionSprite ])

displayPlay : Model -> List (Svg Msg)
displayPlay ({state, score, ship, projectiles, enemies, explosions, enemyProjectiles} as game) =
    [ rect [ fill "black"
           , x "0"
           , y "0"
           , width (toString gameWidth ++ "px")
           , height (toString gameHeight ++ "px") ] [] ] ++
    (map displayProjectile projectiles) ++
    (map displayEnemy enemies) ++
    (map displayExplosion explosions) ++
    (map displayProjectile enemyProjectiles) ++
    [ displayShip state ship
    , text' [ x "10", y "20", textAnchor "start", fill "white" ] [ text ("SCORE: " ++ toString score) ]
    --, text' [ x "10", y "40", textAnchor "start" ] [ text (toString rate ++ " fps") ]
    ]


tweetLink : Int -> String
tweetLink score =
    let base = "https://twitter.com/intent/tweet?text="
        --HACK: pre-encoded
        text = String.join "" [
            "I%20scored%20",
            toString score,
            "%20on%20the%20%40FireflyLogic%20404%20game!%20http%3A%2F%2Ffireflylogic.com%2F404%20%23pewpew"
        ]
    in base ++ text

displayGameOver : String -> Model -> List (Svg Msg)
displayGameOver message ({score} as model) =
    displayPlay model ++
    [ rect [ x "0"
           , y "0"
           , width (toString gameWidth)
           , height (toString gameHeight)
           , fill "rgba(0, 0, 0, 0.5)" ] []
    , text' [ x (toString halfWidth)
            , y (toString (halfHeight - 30))
            , textAnchor "middle"
            , fontSize "50px"
            , fill "white" ] [ text message ]
    , a [ xlinkHref (tweetLink score) ]
        [ text' [ x (toString halfWidth)
                , y (toString (halfHeight + 50))
                , textAnchor "middle"
                , fontSize "20px"
                , fill "blue" ] [ text "Tweet My Score" ] ]
    ]

view : Model -> Html Msg
view model =
  div [][
  svg [ viewBox ("0 0 " ++ toString gameWidth ++ " " ++ toString gameHeight)
      , width (toString gameWidth ++ "px")
      ]
      (case model.state of
          Play -> displayPlay model
          Win  -> displayGameOver "You Win!" model
          Lose -> displayGameOver "Good try!" model)
  ]
