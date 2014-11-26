module PewPew.Level where

import String
import PewPew.Model as Model


starCoordinates: (Int, Int) -> [Char] -> [(Int, Int)]
starCoordinates (row, col) chars = case chars of
  [] -> []
  ('\n'::rest) -> starCoordinates (row + 1, 0) rest
  ('*'::rest) -> (row, col) :: starCoordinates (row, col + 1) rest
  (_::rest) -> starCoordinates (row, col + 1) rest

asciiToEnemies: String -> [Model.Enemy]
asciiToEnemies string =
    let positions = String.toList string |> starCoordinates (0, 0)
        count = length positions
        initialVelocity = Model.enemyVelocity 1 count
        makeEnemy (row, col) = Model.makeEnemy row col initialVelocity
    in map makeEnemy positions

level: [Model.Enemy]
level = asciiToEnemies """
  *      **   *
  * *   *  *  * *
  * *   *  *  * *
  ****  *  *  ****
    *   *  *    *
    *    **     *
"""

fleetSize = length level
