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

cubicEasing: Int -> Float -> Float -> Int -> Float
cubicEasing duration min max t =
   let x = -1.0 + 2.0 * (toFloat t) / (toFloat duration)
       y0 = (min + max) / 2.0
   in y0 + (max - y0) * x^3



-- From Pong in Elm sample (http://elm-lang.org/edit/examples/Intermediate/Pong.elm)
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c
