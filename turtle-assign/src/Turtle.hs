module Turtle(
        Turtle(..), Point(..), Pen(..), Color(..),
        runTurtle, getPosition, setPosition, getDirection, getPen,
        createColor, getColor, setColor, str2Color, applyInt,
        applyPoint, rotateDir, home, forward, backward, left,
        right, penUp, penDown, isPenUp, isPenUpBool, isPenDown,
        isPenDownBool, errorProtocol, initTurtle
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State
import GHC.Float (Floating(pi))


-- For point data, like position, vector, and point
data Point = Point Double Double
    deriving Eq
instance Show Point where
    show (Point a b) = show a ++ ", " ++ show b

-- Direction and position types to make the function requirements clearer
-- however, I never used it
type Direction = Point
type Position = Point

-- Pen state: either Up or Down
data Pen = Up | Down
    deriving (Show, Eq)

-- Color state : r = red, g = green, b = blue, a = alpha
data Color = Color { r :: Int,
                     g :: Int,
                     b :: Int,
                     a :: Float
                   }
    deriving (Eq, Read)
instance Show Color where
    show (Color r g b a) = "rgba(" ++ show r ++ ", " ++
                                      show g ++ ", " ++
                                      show b ++ ", " ++
                                      show a ++ ")"

-- Turtle state : pos = current position, 
--                dir = facing direction,
--                pen = pen state,
--                color = color state
data Turtle = Turtle {
                        pos :: Point,
                        dir :: Point,
                        pen :: Pen,
                        color :: Color
                    }
    deriving (Show)

runTurtle = runStateT

-- Getters and setters for turtle attributes
getPosition :: Turtle -> Point
getPosition = pos

setPosition :: Turtle -> Point -> Turtle
setPosition tr p = tr {pos = p}

getDirection :: Turtle -> Point
getDirection = dir

getPen :: Turtle -> Pen
getPen = pen

-- Color creation from RGBA numbers
createColor :: Int -> Int -> Int -> Float -> Color
createColor r g b a | r > 256 || r < 0 ||
                      g > 256 || g < 0 ||
                      b > 256 || b < 0 ||
                      a > 1 || a < 0 = black
                    | otherwise = Color r g b a

-- Color creation from string name
black = createColor 0 0 0 1
red = createColor 255 0 0 1
green = createColor 0 255 0 1
blue = createColor 0 0 255 1
yellow = createColor 255 255 0 1

str2Color :: String -> Maybe Color
str2Color str =
    case str of
        "black" -> Just black
        "red" -> Just red
        "yellow" -> Just yellow
        "blue" -> Just blue
        "green" -> Just green
        _ -> Nothing

getColor :: Turtle -> Color
getColor = color

setColor :: Color -> StateT Turtle Maybe String
setColor clr = do
    tr <- get
    put tr {color = clr}
    lift (Just (
        "Color set to "++ show clr ++ ".\n"))

-- Point and vector (direction) manipulation functions
-- Function to apply an Int to a point's coordinates according to a function
applyInt :: (Num b) => Point -> (Double -> b -> Double) -> Int -> Point
applyInt (Point x y) f n =
    let d = fromIntegral n
        newX = f x d
        newY = f y d
    in Point newX newY
-- Function to apply a function onto corresponding coordinates of two points
applyPoint :: Point -> (Double -> Double -> Double) -> Point -> Point
applyPoint (Point ax ay) f (Point bx by) =
    let newX = f ax bx
        newY = f ay by
    in Point newX newY

-- Function to get the unit vector of a vector
unitDir :: Direction -> Direction
unitDir (Point x y) = let mag = sqrt ( x * x + y * y)
                      in Point (x / mag) (y / mag)

-- Function to change the position of a Turtle
changePos :: Turtle -> Int -> Turtle
changePos attr units =
    let unitdir = unitDir (dir attr)
        dist = applyInt unitdir (*) units
        newPos = applyPoint dist (+) (pos attr)
    in attr {pos = newPos}

-- Function to change the facing direction of a Turtle
rotateDir :: Turtle -> Double -> Turtle
rotateDir attr degrees =
    let angle = degrees * (pi/180)
        Point x y = unitDir $ dir attr
        newX = x * cos angle - y * sin angle
        newY = x * sin angle + y * cos angle
    in attr {dir = unitDir $ Point newX newY}

-- Initial Turtle at position (0, 0), facing up (0, 1), with the pen down and the color set to black
initTurtle = Turtle {pos = Point 0 0,
                dir = unitDir (Point 0 1),
                pen = Down,
                color = black
               }

-- Function to set the Turtle state to the initial Turtle
home :: StateT Turtle Maybe String
home = do
    put initTurtle
    lift (Just "Turtle is set to center position facing north, pen set down and color to black.\n")

-- Change the Turtle state by moving the turtle's position forward
forward :: Int -> StateT Turtle Maybe String
forward units = do
    tr <- get
    put (changePos tr (units * (-1)))
    lift (Just ("Turtle moved forward by " ++ show units ++ " units.\n"))


-- Change the Turtle state by moving the turtle's position backward
backward :: Int -> StateT Turtle Maybe String
backward units = do
    tr <- get
    put (changePos tr units)
    lift (Just ("Turtle moved backward by " ++ show units ++ " units.\n"))


-- Change the Turtle state by turning the turtle's facing direction to the left
left :: Double -> StateT Turtle Maybe String
left angle = do
    tr <- get
    put (rotateDir tr (angle * (-1)))
    lift (Just ("Facing direction is rotated to the left by " ++ show angle ++ " degrees.\n"))

-- Change the Turtle state by turning the turtle's facing direction to the right
right :: Double -> StateT Turtle Maybe String
right angle = do
    tr <- get
    put (rotateDir tr angle)
    lift (Just ("Facing direction is rotated to the right by " ++ show angle ++ " degrees.\n"))

-- Change the pen position to Up or Down
penUp :: StateT Turtle Maybe String
penUp = do
    tr <- get
    put tr {pen = Up}
    lift ( Just "Pen is up for moving without drawing.\n")

penDown :: StateT Turtle Maybe String
penDown = do
    tr <- get
    put tr {pen = Down}
    lift ( Just "Pen is down for drawing.\n" )

-- Functions to check the turtle's pen state returning a monad transformer
-- maybe bool or a simple bool
isPenUp :: StateT Turtle Maybe Bool
isPenUp = do
    tr <- get
    lift (Just (pen tr == Up))
isPenUpBool :: Turtle -> Bool
isPenUpBool tr = pen tr == Up

isPenDown :: StateT Turtle Maybe Bool
isPenDown = do
    tr <- get
    lift (Just (pen tr == Down))
isPenDownBool :: Turtle -> Bool
isPenDownBool tr = not (isPenUpBool tr)

-- An error protocol to reset the Turtle state
errorProtocol :: [Char] -> Turtle -> StateT Turtle Maybe String
errorProtocol str tr = do
    () <- put tr
    lift (Just ("Error occured:\n" ++ str ++ "\nTurtle reinitiated to last working position\n"))
