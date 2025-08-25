module Draw_1 (
    takeCommand
) where

import Turtle

takeCommand = do
    runTurtle (home >> forward 10 >> left 40) undefined