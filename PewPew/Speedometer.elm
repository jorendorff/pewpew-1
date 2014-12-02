-- Speedometer.elm: Code for measuring a signal's update rate.
module PewPew.Speedometer where

import List
import Time(timestamp, inSeconds)

-- Fast, concise FIFO queue implementation based on Figure 5.2 of Okasaki,
-- "Purely Functional Data Structures".
--
-- A queue is implemented as two lists: a front and a back. The back list is
-- stored in reverse order. The code relies on one unobvious invariant: if the
-- front list is empty, the back list must also be empty.
--
-- split is a classier replacement for isEmpty, head, and tail.

type Queue a = ([a], [a])
empty = ([], [])
checkf q = case q of
    ([], r) -> (reverse r, [])
    _ -> q
snoc (f, r) x = checkf (f, x::r)
split q = case q of
    ([], _) -> Nothing
    (x::f, r) -> Just (x, checkf (f, r))


-- dropWhile variant that also returns the number of items dropped.
dropWhile : (a -> Bool) -> Queue a -> (Int, Queue a)
dropWhile pred q =
    let dropper n q' = case split q' of
        Nothing -> (n, q')
        Just (h, t) -> if pred h then dropper (n + 1) t else (n, q')
    in dropper 0 q

-- Compute how often a given signal is being updated, in updates per second.
--
-- The `window` parameter tells how long a window of updates to take into
-- account.  If in doubt, use Time.second.
--
-- Caveats:
--
-- 1.  This *only* looks at the given signal, so it will not detect if the
--     signal is updating rapidly but the browser is dropping frames.  I think
--     I have observed this happening while the reported frame rate was >50,
--     indicating it's a problem in practice.
--
-- 2.  The algorithm only updates immediately after the given signal updates.
--     This means the frame rate will never be reported as less than `1/window`
--     no matter how bad it is.
--
updateRate: Time -> Signal a -> Signal Float
updateRate window signal =
    let windowSeconds = inSeconds window
        update : (Time, a) -> (Queue Time, Int) -> (Queue Time, Int)
        update (now, _) (q, len) =
            let w0 = now - window
                (ndropped, q') = dropWhile ((>=) w0) (snoc q now)
                len' = len + 1 - ndropped
            in (q', len')
    in timestamp signal
        |> foldp update (empty, 0)
        |> lift (\(_, len) -> toFloat len / windowSeconds)
