module Turtle(
        someFunc,
        Turtle,
        runTurtle,
        home,
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 05.01 IS GOOD TO LOOK AT FOR THIS ASSIGNMENT

-- For point data, like goal position
data Point = Point Double Double

-- Position of the turtle (a point basically)
-- Not used, for now
type Position = Point

-- Direction the turle facing at (a vector basically)
-- Not used, for now
type Direction = Point

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

-- Pen state: either Up or Down
data Pen = Up | Down

data Color = Black | Red | Green | Blue | Yellow
    deriving Show
black = Black
red = Red
green = Green
blue = Blue
yellow = Yellow

data TurtleAttr = TurtleAttr {
                        pos :: Point,
                        dir :: Point,
                        pen :: Pen,
                        color :: Color
                  }

newtype Turtle st a = Turtle (TurtleAttr -> (a, TurtleAttr))

instance Functor (Turtle TurtleAttr) where
    fmap f a = do f <$> a

instance  Applicative (Turtle TurtleAttr) where
    pure a = Turtle (\t -> (a, t))

    tf <*> tx = Turtle (\st0 ->
                    let (f, st1) = runTurtle tf st0
                        (x, st2) = runTurtle tx st1
                    in (f x, st2))

instance Monad (Turtle TurtleAttr) where
    tx >>= f = Turtle (\st0 ->
                    let (x, st1) = runTurtle tx st0
                    in runTurtle (f x) st1)

runTurtle :: Turtle TurtleAttr a -> TurtleAttr -> (a, TurtleAttr)
runTurtle (Turtle f) = f

unitDir :: Direction -> Direction
unitDir (Point x y) = let mag = sqrt ( x * x + y * y)
                      in Point (x / mag) (y / mag)

home :: Turtle a String
home = Turtle (\t -> 
                ("Turtle is set to center position facing north.\n",
                 t {pos = Point 0 0, 
                    dir = unitDir (Point 0 1), 
                    pen = Down, 
                    color = black
                   }
                )
              )

changePos :: TurtleAttr -> Int -> TurtleAttr
changePos attr units =
    let unitdir = unitDir (dir attr)
        dist = applyInt unitdir (*) units
        newPos = applyPoint dist (+) (pos attr)
    in attr {pos = newPos}
forward :: Int -> Turtle a String
forward units = Turtle (\attr -> ("Turtle moved forward by " ++ show units ++ " units.\n", changePos attr units))
backward :: Int -> Turtle a String
backward units = Turtle (\attr -> ("Turtle moved backwards by " ++ show units ++ " units.\n", changePos attr (units * (-1))))

rotateDir :: TurtleAttr -> Double -> TurtleAttr
rotateDir attr angle =
    let Point x y = dir attr
        newX = x * cos angle - y * sin angle
        newY = x * sin angle + y * cos angle
    in attr {dir = Point newX newY}
left :: Double -> Turtle a String
left angle = Turtle (\attr -> 
    ("Facing direction is rotated to the right by " ++ show angle ++ " degrees.\n", rotateDir attr angle))
right :: Double -> Turtle a String
right angle = Turtle (\attr -> 
    ("Facing direction is rotated to the right by " ++ show angle ++ " degrees.\n", rotateDir attr (angle * (-1))))

penUp :: Turtle a String
penUp = Turtle (\t -> ("Pen is up for moving without drawing.\n", t {pen = Up}))
penDown :: Turtle a String
penDown = Turtle (\t -> ("Pen is down for drawing.\n", t {pen = Down}))

setColor :: Color -> Turtle a String
setColor color = Turtle (\t -> ("Color set to "++ show color ++ ".\n", (t {color = color})))


-- Rule creation for fractals
data Rule = Rule {from :: String,
                  to :: String}

type Rules = [Rule]

{-
  lSystem :: Rules -> String -> Int -> Turtle ()
  lSystem (Rules r) axiom i = 
-}