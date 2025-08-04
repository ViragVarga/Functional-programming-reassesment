module Draw (
    someFunc
) where

import Turtle

someFunc :: IO ()
someFunc = putStrLn "Some Function"

myTurtle = runTurtle home

takeCommand = 
    -- get chars -> string
    -- check strings