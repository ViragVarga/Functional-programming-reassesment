{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Fractals (Fractal(..), lSystem) where

import Turtle (Turtle(..), Point(..), Pen(..), forward, right, left, runTurtle)
import Data.List (iterate)
import Distribution.Simple.Utils
import Control.Monad.State
import Data.IORef

type Rule = (Char, String)
type Rules = [Rule]
data Fractal = Fractal {rules :: Rules,
                        degrees :: Double,
                        step :: Int}

newtype Stack s a = Stack {runStack :: [(Point, Point)] -> (a, [(Point, Point)])}

push :: (Point, Point) -> Stack s ()
push x = Stack $ \xs -> ((), x:xs)

--pop :: Stack s (Maybe (Point, Point))
pop = Stack $ \st -> case st of
    ((Point x1 y1, Point x2 y2):xs) -> (Just (Point x1 y1, Point x2 y2), xs)
    _ -> (Nothing, [])
--runStack = runState

-- do localStack <- liftIO $ newIORef [(Point 0 0, Point 0 0)]

-- push :: (Point, Point) -> m ()
-- push newEntry = do
--     liftIO $ modifyIORef' localStack ([newEntry] ++)
-- 
-- --pop :: (Point, Point)
-- pop = do
--     (x:xs) <- liftIO $ readIORef localStack 
--     liftIO $ writeIORef localStack xs
--     x


applyRules2Char :: [Rule] -> Char -> String
applyRules2Char [] ch = [ch]
applyRules2Char ((from, to):rules) ch
    | from == ch = to
    | otherwise = applyRules2Char rules ch

applyRules :: [Rule] -> String -> String
applyRules _ [] = ""
applyRules [] str = str
applyRules ((from, to):rules) (ch:chs)
    | from == ch = to ++ applyRules ((from, to):rules) chs
    | otherwise = applyRules2Char rules ch ++ applyRules ((from, to):rules) chs

lSystem :: Fractal -> String -> Int -> Turtle -> [Turtle] -- Turtle
lSystem frac axiom it tr =
    if it < 0
        then []
        else do
            case safeLast $ take (it + 1) $ iterate (applyRules (rules frac)) axiom of
                Nothing -> []
                Just instructions ->
                    interpret instructions (degrees frac) (step frac) [] tr
           

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