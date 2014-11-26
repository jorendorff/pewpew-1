module PewPew.Utils where

withIndex list = list |> zip [0..length list]

throttle: Signal Bool -> Time -> Signal Bool
throttle input interval=
    let hz = fps 60
        sampled = timestamp <| sampleOn hz input
        throttle' (t,input) (_, tLast) = if
            | input && t-tLast > interval  -> (True,t)
            | otherwise -> (False, tLast)
    in
        fst <~ foldp throttle' (False,0) sampled

cubicEasing: (Int, Float) -> (Int, Float) -> Int -> Float
cubicEasing (x0, y0) (x1, y1) x =
    let nx = -1.0 + 2.0 * toFloat (x - x0) / toFloat (x1 - x0)
        ymid = (y0 + y1) / 2.0
    in ymid + (y1 - ymid) * nx^3



-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c
