module PewPew.Utils where

withIndex list = zip [0..length list] list

cubicEasing: (Int, Float) -> (Int, Float) -> Int -> Float
cubicEasing (x0, y0) (x1, y1) x =
    let nx = -1.0 + 2.0 * toFloat (x - x0) / toFloat (x1 - x0)
        ymid = (y0 + y1) / 2.0
    in ymid + (y1 - ymid) * nx^3


-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c
