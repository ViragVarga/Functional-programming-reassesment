module Turtle(
        Turtle(..), Point(..), Pen, Color(..),
        runTurtle, getPosition, setPosition, getDirection, getPen,
        createColor, getColor, setColor, str2Color, applyInt,
        applyPoint, rotateDir, home, forward, backward, left,
        right, penUp, penDown, isPenUp, isPenUpBool, isPenDown,
        isPenDownBool, errorProtocol
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.State
import GHC.Float (Floating(pi))


data Turtle = Turtle {
                        pos :: Point,
                        dir :: Point,
                        pen :: Pen,
                        color :: Color
                    }
    deriving (Show)

-- For point data, like position, vector, and point
data Point = Point Double Double
    deriving Eq
instance Show Point where
    show (Point a b) = show a ++ ", " ++ show b

type Direction = Point
type Position = Point

-- Pen state: either Up or Down
data Pen = Up | Down
    deriving (Show, Eq)

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
black = Color 0 0 0 1
red = Color 255 0 0 1
green = Color 0 255 0 1
blue = Color 0 0 255 1
yellow = Color 255 255 0 1


runTurtle = runStateT

getPosition :: Turtle -> Point
getPosition = pos

setPosition :: Turtle -> Point -> Turtle
setPosition tr p = tr {pos = p}

getDirection :: Turtle -> Point
getDirection = dir

getPen :: Turtle -> Pen
getPen = pen

createColor :: Int -> Int -> Int -> Float -> Color
createColor r g b a | r > 256 || r < 0 ||
                      g > 256 || g < 0 ||
                      b > 256 || b < 0 ||
                      a > 1 || a < 0 = black
                    | otherwise = Color r g b a

getColor :: Turtle -> Color
getColor = color

setColor :: Color -> StateT Turtle Maybe String
setColor clr = do
    tr <- get
    put tr {color = clr}
    lift (Just (
        "Color set to "++ show clr ++ ".\n"))

applyInt :: (Num b) => Point -> (Double -> b -> Double) -> Int -> Point
applyInt (Point x y) f n =
    let d = fromIntegral n
        newX = f x d
        newY = f y d
    in Point newX newY
applyPoint :: Point -> (Double -> Double -> Double) -> Point -> Point
applyPoint (Point ax ay) f (Point bx by) =
    let newX = f ax bx
        newY = f ay by
    in Point newX newY

str2Color :: String -> Maybe Color
str2Color str =
    case str of
        "black" -> Just black
        "red" -> Just red
        "yellow" -> Just yellow
        "blue" -> Just blue
        "green" -> Just green
        _ -> Nothing

unitDir :: Direction -> Direction
unitDir (Point x y) = let mag = sqrt ( x * x + y * y)
                      in Point (x / mag) (y / mag)

changePos :: Turtle -> Int -> Turtle
changePos attr units =
    let unitdir = unitDir (dir attr)
        dist = applyInt unitdir (*) units
        newPos = applyPoint dist (+) (pos attr)
    in attr {pos = newPos}

rotateDir :: Turtle -> Double -> Turtle
rotateDir attr degrees =
    let angle = degrees * (pi/180)
        Point x y = unitDir $ dir attr
        newX = x * cos angle - y * sin angle
        newY = x * sin angle + y * cos angle
    in attr {dir = unitDir $ Point newX newY}

initTurtle = Turtle {pos = Point 0 0, 
                dir = unitDir (Point 0 1), 
                pen = Down, 
                color = black
               }

home :: StateT Turtle Maybe String
home = do
    put initTurtle
    lift (Just "Turtle is set to center position facing north.\n")


forward :: Int -> StateT Turtle Maybe String
forward units = do 
    tr <- get
    put (changePos tr (units * (-1)))
    lift (Just ("Turtle moved forward by " ++ show units ++ " units.\n"))


backward :: Int -> StateT Turtle Maybe String
backward units = do 
    tr <- get
    put (changePos tr units)
    lift (Just ("Turtle moved backward by " ++ show units ++ " units.\n"))


left :: Double -> StateT Turtle Maybe String
left angle = do
    tr <- get 
    put (rotateDir tr (angle * (-1))) 
    lift (Just ("Facing direction is rotated to the left by " ++ show angle ++ " degrees.\n"))

right :: Double -> StateT Turtle Maybe String
right angle = do
    tr <- get
    put (rotateDir tr angle) 
    lift (Just ("Facing direction is rotated to the right by " ++ show angle ++ " degrees.\n"))
    

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
isPenDownBool tr = pen tr == Down

errorProtocol :: [Char] -> Turtle -> StateT Turtle Maybe String
errorProtocol str tr = do
    () <- put tr
    lift (Just ("Error occured:\n" ++ str ++ "\nTurtle reinitiated to last working position\n"))

{-
  -- Rule creation for fractals
  data Rule = Rule {from :: String,
                    to :: String}
  
  type Rules = [Rule]
  
  
  lSystem :: Rules -> String -> Int -> turtleog ()
  lSystem (Rules r) axiom i = 
-}