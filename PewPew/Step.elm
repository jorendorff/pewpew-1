module PewPew.Step where

import PewPew.Input (..)
import PewPew.Model (..)


stepObj : Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * t
          , y <- y + vy * t }

stepShip : Time -> Int -> Ship -> Ship
stepShip t dir ship =
    let shipWidth = 40
        vx'       = toFloat dir * 400
        ship'     = stepObj t { ship | vx <- vx'  }
        x'        = clamp (shipWidth/2-halfWidth) (halfWidth-shipWidth/2) ship'.x
  in
      { ship' | x <- x'}

isOnScreen projectile = projectile.y < halfHeight

stepProjectiles : Time -> Bool -> Float -> [Projectile] -> [Projectile]
stepProjectiles t firing origin projectiles =
    let projectiles' =  projectiles |> map (stepObj t) |> filter isOnScreen
    in case firing of
        True -> { x=origin, y=20-halfHeight, vx = 0, vy=400} :: projectiles'
        _ -> projectiles'

stepEnemies : Time -> [Enemy] -> [Enemy]
stepEnemies t enemies =
    let enemies'   = enemies |> map (stepObj t)
        count      = length enemies'
        positions  = map .x enemies'
        (low,high) = (minimum positions, maximum positions)
        dir        = if
                        | low + halfWidth <= 30 -> Just 1
                        | halfWidth - high < 30 -> Just -1
                        | otherwise  -> Nothing

    in case dir of
         Just v -> map (\enemy -> { enemy | vx <- enemyVelocity v count }) enemies'
         _      -> enemies'

-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c

-- is the ball within a paddle?
within : (Projectile,Enemy) -> Bool
within (projectile, enemy) =
    (projectile.x |> near enemy.x 14) && (projectile.y |> near enemy.y 8)

except: [a] -> [a] -> [a]
except a b =
    let inB x = any ((==) x) b
    in filter (not . inB) a

stepCollisions: [Projectile] -> [Enemy] -> ([Projectile],[Enemy],[Explosion])
stepCollisions projectiles enemies =
    let hits = projectiles |> concatMap ((flip map enemies) . (,)) |> filter within
        (hitProjectiles, hitEnemies) = unzip hits
        explosions = hitEnemies |> map (\enemy -> {time = inSeconds 150 * millisecond, vx = enemy.vx / 1.2 , vy = 0, x = enemy.x, y = enemy.y})
    in
        (projectiles `except` hitProjectiles, enemies `except` hitEnemies, explosions)

stepExplosions: Time -> [Explosion] -> [Explosion]
stepExplosions t explosions =
    let burn e = {e | time <- e.time - t}
    in explosions |> map (stepObj t) |> map burn |> filter ((<) 0 . .time)


stepPlay : Input -> Game -> Game
stepPlay {firing, direction, delta} ({ship, projectiles, enemies, explosions} as game)=
    let ship'        = stepShip delta direction ship
        projectiles' = stepProjectiles delta firing ship.x projectiles
        enemies'     = case enemies of
                        [] -> enemies
                        _  -> stepEnemies delta enemies
        (projectiles'',enemies'', explosions') = stepCollisions projectiles' enemies'
        explosions'' = stepExplosions delta explosions ++ explosions'
        state'       = case enemies of
                        [] -> Win
                        _  -> Play
    in
        {game |
            ship <- ship',
            projectiles <- projectiles'',
            enemies <- enemies'',
            explosions <- explosions'',
            state <- state'
        }

next : Input -> Game -> Game
next input ({state} as game)=
    case state of
        Play -> stepPlay input game
        Win  -> game
