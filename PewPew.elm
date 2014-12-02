module PewPew where

import PewPew.Input (input)
import PewPew.Model (defaultGame)
import PewPew.Level as Level
import PewPew.Step as Step
import PewPew.View as View
import PewPew.Speedometer (updateRate)
import Window

game  = { defaultGame | enemies <- Level.level}
state = foldp Step.next game input
fps = updateRate second state
main  = View.display <~ Window.dimensions ~ lift2 (,) state fps
