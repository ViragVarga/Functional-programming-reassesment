module Draw_1 (
    someFunc
) where

import Turtle

someFunc :: IO ()
someFunc = putStrLn "Some Function"


command = "forward 10\n" ++
          "left 40\n" ++
          "pen up\n" ++
          "backward 30\n" ++
          "right 100\n" ++
          "pen down\n" ++
          "setColor green\n" ++
          "forward 40\n"

-- while :: Monad m => Bool -> m a -> m ()
while b f
    | b       = f >> while b f
    | otherwise = return ()

drawTurtle :: Turtle -> IO ()
drawTurtle tr = do
    putStrLn ("\n\nCurrent position:\t" ++ show (getPosition tr) ++
              "\nCurrent direction:\t" ++ show (getDirection tr) ++
              "\nPen position:\t" ++ show (getPen tr) ++
              "\nCurrent color:\t" ++ show (getColor tr)
             )


takeCommand = do
    (str, myTurtle) <- runTurtle home undefined
    runTurtle (forward 10 >> left 40) myTurtle 

{-
takeCommand =
    let ch = getChar
        newLine = return '\n'
        space = return ' '
        str = return ""
        (turtleStr, myTurtle) = runTurtle home undefined
    in while (ch /= newLine) (
        do
         if ch /= space
             then update str (str ++ [ch])
             else do
                drawTurtle myTurtle
                case str of
                    ["forward"] 
                     -> let unit = getLine
                        in runTurtle (forward (toInteger unit)) myTurtle
                    "backward" ->
                        let unit = getLine
                        in runTurtle (backward (toInteger unit)) myTurtle
                    "left" ->
                        let angle = getLine
                        in runTurtle (left (fromInteger (toInteger angle))) myTurtle
                    "right" ->
                        let angle = getLine
                        in runTurtle (right (fromInteger (toInteger angle))) myTurtle
                    "pen" -> do
                        state <- getLine
                        case state of
                            "up" -> runTurtle penUp myTurtle
                            "down" -> runTurtle penDown myTurtle
                    "setColor" -> do
                        color <- getLine
                        runTurtle (setColor color) myTurtle
                    _ -> myTurtle (\st -> errorProtocol "Input command error" st)
         ch <- getChar
         myTurtle (\tr -> drawTurtle tr)
        )
-}