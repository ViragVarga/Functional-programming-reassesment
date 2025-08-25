{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Fractals (Fractal(..), lSystem) where

import Turtle (Turtle(..), Point(..), Pen(..), forward, right, left, runTurtle)
import Data.List (iterate)
import Distribution.Simple.Utils
import Control.Monad.State
import Data.IORef

-- new data types to contain information about L-System and Fractal attributes
type Rule = (Char, String)
type Rules = [Rule]
data Fractal = Fractal {rules :: Rules,
                        degrees :: Double,
                        step :: Int}

-- Stack element initiation
newtype Stack s a = Stack {runStack :: [(Point, Point)] -> (a, [(Point, Point)])}

-- Push and pop functions for stack element
push :: (Point, Point) -> Stack s ()
push x = Stack $ \xs -> ((), x:xs)

pop = Stack $ \st -> case st of
    ((Point x1 y1, Point x2 y2):xs) -> (Just (Point x1 y1, Point x2 y2), xs)
    _ -> (Nothing, [])


-- Function to apply a list of rules to a character
applyRules2Char :: [Rule] -> Char -> String
applyRules2Char [] ch = [ch]
applyRules2Char ((from, to):rules) ch
    | from == ch = to
    | otherwise = applyRules2Char rules ch

-- Function to apply a list of rules to a list of characters
applyRules :: [Rule] -> String -> String
applyRules _ [] = ""
applyRules [] str = str
applyRules ((from, to):rules) (ch:chs)
    | from == ch = to ++ applyRules ((from, to):rules) chs
    | otherwise = applyRules2Char rules ch ++ applyRules ((from, to):rules) chs

-- L-System function that takes a Fractal (containing the rules, default
-- turn angle and muvement unit), an axiom, an Interation number, and an 
-- initial turtle state and returns a list of turtle states in order of change
lSystem :: Fractal -> String -> Int -> Turtle -> [Turtle]
lSystem frac axiom it tr =
    if it < 0
        then []
        else do
            case safeLast $ take (it + 1) $ iterate (applyRules (rules frac)) axiom of
                Nothing -> []
                Just instructions ->
                    interpret instructions (degrees frac) (step frac) [] tr
           
-- Function to interpret the instruction string, apply it and return a list of turtle states
interpret :: String -> Double -> Int -> [(Point, Point)] -> Turtle -> [Turtle]
interpret [] _ _ _ _= []
interpret (x:xs) degrees units stack tr =
    case x of
        'F' -> do
            case runTurtle (forward units) tr of
                Nothing -> []
                Just (_, newTr) -> do
                    newTr {pen = Down} : interpret xs degrees units stack newTr
        'G' -> do
            case runTurtle (forward units) tr of
                Nothing -> []
                Just (_, newTr) -> do
                    newTr {pen = Down} : interpret xs degrees units stack newTr
        'f' -> do
            case runTurtle (forward units) tr of
                Nothing -> []
                Just (_, newTr) -> do
                    newTr {pen = Up} : interpret xs degrees units stack newTr
        'g' -> do
            case runTurtle (forward units) tr of
                Nothing -> []
                Just (_, newTr) -> do
                    newTr {pen = Up} : interpret xs degrees units stack newTr
        '+' -> do
            case runTurtle (left degrees) tr of
                Nothing -> []
                Just (_, newTr) -> do
                    newTr {pen = Down} : interpret xs degrees units stack newTr
        '-' -> do
            case runTurtle (right degrees) tr of
                Nothing -> []
                Just (_, newTr) -> do
                    newTr {pen = Down} : interpret xs degrees units stack newTr
        '[' -> do
            case runStack (push (pos tr, dir tr)) stack of
                (_, []) -> []
                (_, st) -> interpret xs degrees units st tr
        ']' -> do
            case runStack pop stack of
                (Just (newPos, newDir), st) -> do
                    newTr <- [Turtle {pos = newPos, dir = newDir, pen = Up, color = color tr}]
                    newTr : interpret xs degrees units st newTr
                (Nothing, st) -> do
                    tr : interpret xs degrees units st tr
        _ -> interpret xs degrees units stack tr