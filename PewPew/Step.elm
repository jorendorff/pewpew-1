module PewPew.Step where

import List
import PewPew.Input (Input)
import PewPew.Model (..)
import PewPew.Utils as Utils
import PewPew.Level as Level

--
--Helper functions
--

tickDuration = 0.001  -- seconds
reloadDuration = 0.350  -- seconds

-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
stepObj : Object a -> Object a
stepObj ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * tickDuration
          , y <- y + vy * tickDuration }

isOnScreen : Object a -> Bool
isOnScreen {x,y} =
    y < halfHeight
    && y > -halfHeight
    && x < halfWidth
    && x > -halfWidth

within : Float -> Float -> Object a -> Object b -> Bool
within x y a b =
     (a.x |> Utils.near b.x x) && (a.y |> Utils.near b.y y)

except: [a] -> [a] -> [a]
except a b =
    let inB x = any ((==) x) b
    in filter (not << inB) a


--
-- Model specific step functions
--

stepShip : Int -> Ship -> Ship
stepShip dir ship =
    let shipWidth = 40
        vx' = toFloat dir * 400
        ship' = stepObj { ship | vx <- vx'  }
        x' = clamp (shipWidth/2-halfWidth) (halfWidth-shipWidth/2) ship'.x

    in
      { ship' | x <- x'}


stepProjectiles : Bool -> Float -> [Projectile] -> [Projectile]
stepProjectiles firing origin projectiles =
    let projectiles' = projectiles
        |> map stepObj
        |> filter isOnScreen

    in case firing of
        True -> { x=origin, y=20-halfHeight, vx = 0, vy=400 } :: projectiles'
        _ -> projectiles'


stepEnemies : [Enemy] -> [Enemy]
stepEnemies enemies =
    case enemies of
        [] -> []
        (first::rest) ->
            let enemies' = enemies
                            |> map stepObj
                            |> map (\e -> {e | lastFired <- e.lastFired + tickDuration})
                count = length enemies'
                positions = map .x enemies'
                (low,high) = (minimum positions, maximum positions)
                (onEdge, dir) = if
                        | low <= -halfWidth + 30 -> (True, 1)
                        | high > halfWidth - 30 -> (True, -1)
                        | otherwise  -> (False, if first.vx > 0 then 1 else -1)

            in map (\enemy -> { enemy | y <- if onEdge then enemy.y - 8 else enemy.y,
                                        vx <- enemyVelocity dir count Level.fleetSize }) enemies'


shouldFire : Int -> Enemy -> Int -> Bool
shouldFire enemiesRemaining enemy index =
    let interval = Utils.cubicEasing (Level.fleetSize, 10.0) (1, 1.0) enemiesRemaining
        wobble   = abs(tan(toFloat index)) * (toFloat enemiesRemaining) / 2

    in enemy.lastFired > interval + wobble


tryEnemyFire : Int -> (Int,Enemy) -> (Enemy, Maybe Projectile)
tryEnemyFire enemiesRemaing (index,enemy) =
    case shouldFire enemiesRemaing enemy index  of
    True  -> ({enemy| lastFired <- 0},
              Just {
                  x = enemy.x,
                  y = enemy.y,
                  vy = -10,
                  vx = enemy.vx })
    False -> (enemy,Nothing)

-- Acceleration of gravity (determined experimentally)
g = 158.0

-- Horizontal wind braking factor
braking = 0.3

accelerateProjectile : Projectile -> Projectile
accelerateProjectile p = {p | vx <- p.vx * (braking ^ tickDuration)
                            , vy <- p.vy - g * tickDuration}

stepEnemyFire : [Projectile] -> [Enemy] -> ([Enemy],[Projectile])
stepEnemyFire projectiles enemies =
    let indexed = enemies |> Utils.withIndex
        projectiles' = projectiles
                        |> map stepObj
                        |> filter isOnScreen
                        |> map accelerateProjectile

        (enemies',newProjectiles) = indexed
                                    |> map (tryEnemyFire (length enemies))
                                    |> unzip
        enemies'' = enemies' |> map (\enemy -> {enemy | lastFired <- enemy.lastFired + tickDuration})
        projectiles'' = (newProjectiles |> (List.filterMap identity)) ++ projectiles'

    in (enemies'', projectiles'')


stepEnemyCollisions: [Projectile] -> [Enemy] -> ([Projectile],[Enemy],[Explosion])
stepEnemyCollisions projectiles enemies =
    let hits = projectiles |> concatMap ((flip map enemies) << (,)) |> filter (uncurry (within 14 8))
        (hitProjectiles, hitEnemies) = unzip hits
        explosions = hitEnemies
            |> map (\enemy -> {
                time = inSeconds 150 * millisecond,
                vx = enemy.vx / 1.2 ,
                vy = 0,
                x = enemy.x,
                y = enemy.y
            })
    in
        (projectiles `except` hitProjectiles, enemies `except` hitEnemies, explosions)


stepExplosions: [Explosion] -> [Explosion]
stepExplosions explosions =
    let burn e = {e | time <- e.time - tickDuration}
    in explosions
        |> map stepObj
        |> map burn
        |> filter ((<) 0 << .time)


pointsScored: Time -> Int -> Int
pointsScored duration enemiesHit =
    enemiesHit * max (truncate (300 - duration)) 1

stepPlay : Input -> Game -> Game
stepPlay {firing, direction} ({score, duration, ship, projectiles, enemies, explosions, enemyProjectiles} as game)=
    let ship' : Ship
        ship' = stepShip direction ship
        reallyFiring : Bool
        reallyFiring = firing && duration > game.lastPlayerFireTime + reloadDuration
        projectiles' : [Projectile]
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
            _  -> if
                | enemyProjectiles' |> any (within 12 10 ship') -> Lose
                | enemies' |> any (within 12 10 ship') -> Lose
                | otherwise -> Play

    in
        {game |
            score <- score',
            duration <- duration',
            ship <- ship',
            projectiles <- projectiles'',
            enemies <- enemies'',
            explosions <- explosions'',
            state <- state',
            enemyProjectiles <- enemyProjectiles',
            lastPlayerFireTime <- if reallyFiring then duration' else game.lastPlayerFireTime
        }

advance : Input -> Game -> Game
advance input game =
    let dt = input.delta + game.cachedTime
    in if dt > tickDuration
        then let game' = stepPlay input {game | cachedTime <- 0}
                 input' = {input | delta <- dt - tickDuration}
             in next input' game'
        else {game | cachedTime <- dt}

next : Input -> Game -> Game
next input ({state} as game) =
    case state of
        Play -> advance input game
        Win  -> game
        Lose -> game
