{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Draw_2(
    startDrawingSess
) where

import Text.Read hiding (step)
import GHC.Float (int2Double, double2Float)
import GHC.Unicode (toLower)

import Data.IORef
import Data.List.Extra

import Graphics.UI.Threepenny as UI hiding (Point, map, rules)
import Graphics.UI.Threepenny.Core as Core
import qualified Graphics.UI.Threepenny.SVG as SVG

import Turtle (Point(..), Color(..), Turtle(..), runTurtle, getPosition,
               getDirection, rotateDir, getColor, home, forward, applyInt,
               backward, left, right, penUp, penDown, isPenUpBool, applyPoint,
               str2Color, setColor, setPosition, createColor, errorProtocol)
import Fractals (Fractal(..), lSystem)

canvasHeight :: Int
canvasHeight = 400
canvasWidth :: Int
canvasWidth = 850

myConfig = defaultConfig
{-
    Config { jsPort = Just 2025
           , jsCallBufferMode = NoBuffering
           }
-}

-- myWindow = 

startDrawingSess = startGUI myConfig setup

setup :: Window -> UI ()
setup window = do

    return window # set title "My Turtle game"

    execute <- UI.button #+ [string "Send"]

    command <- UI.input 
    commandIn <- stepper "" $ UI.valueChange command

    fractalSelect <- UI.select
                # set selection (Just 0)
                #+ [UI.option #+ [string "Custom command"]]
                #+ [UI.option #+ [string "Koch Snowflake"]]
                #+ [UI.option #+ [string "Dragon Curve"]]
                #+ [UI.option #+ [string "Sierpinski Triangle"]]
                #+ [UI.option #+ [string "Plant growth"]]
    fractalAngle <- UI.input
                # set type_ "number"
    fractalUnit <- UI.input
                # set type_ "number"
    fractalIteration <- UI.input
                # set type_ "number"
    fractalAxiom <- UI.p
                # set style [("display", "none")]

    restart <- UI.button #+ [string "Restart"]
    clear <- UI.button #+ [string "Clear"]
    logs <- UI.ul # set style [("display", "none"),
                               ("justify-content", "left"),
                               ("overflow", "scroll"),
                               ("max-height", "200px")]
                  #+ [string "Actions taken:"]
                  # set UI.height canvasHeight
                  # set UI.width canvasWidth
    logbox <- UI.input # set UI.type_ "checkbox"

    helpbox <- UI.input # set UI.type_ "checkbox"

    help <- UI.ul # set style [("display", "none"),
                               ("justify-content", "left")]
                  #+ [string "Command name and values______[Description]"]
                  #+ [UI.li # set html "home______[Sets the turtle back to the middle of the canvas facing up]"]
                  #+ [UI.li # set html "forward x______[Move the turtle forward by x units]"]
                  #+ [UI.li # set html "backward x______[Move the turtle backward by x units]"]
                  #+ [UI.li # set html "left x______[Turn the turtle left by x degrees]"]
                  #+ [UI.li # set html "right x______[Turn the turtle right by x degrees]"]
                  #+ [UI.li # set html "pen up/down______[Change the position of the pen. If the pen is up the turtle doesn't draw]"]
                  #+ [UI.li # set html "set color black/red/green/blue/yellow/r g b a______[Change the drawing color of the turtle to black/red/green/blue/yellow or custom color where r, g, b = [0 ... 255] and a = [0 ... 1]]"]

    showBox <- UI.div
            #+ [element helpbox]
            #+ [string "Show/Hide Help"]
            #+ [element logbox]
            #+ [string "Show/Hide Logs"]
            # set style [("display", "flex"),
                         ("justify-content", "center")]

    canvas <- SVG.svg
        # set SVG.width (show canvasWidth)
        # set SVG.height (show canvasHeight)
        # set style [("border", "solid black 1px"),
                     ("background", "#d1d1d1ff")]

    let
        initTrtl tr = setPosition tr (Point (int2Double canvasWidth/2) (int2Double canvasHeight/2))
    case runTurtle home undefined of
        Nothing -> debug "Failed to generate initial turtle state\n"
        Just (initStr, tr) -> do
            myTurtle <- liftIO $ newIORef $ initTrtl tr

            let
                header = "Welcome to my Turtle Game!"
                intro = "You can already see our simple Turtle on the canvas, facing forward. " ++
                        "To make the little triangle move, you need to give commands. " ++
                        "For help with the commands you can check the box below and the acceptable commands appear.\n"

                turtleInFrame :: UI Element
                turtleInFrame = do
                    tr <- liftIO $ readIORef myTurtle
                    let (Point trX trY) = getPosition tr
                    maybePosBox <- getElementById window "posBox"
                    headLine <- UI.p 
                            #+ [string "Turtle position (x, y): "]
                            #+ [string $ show (Point (trX + (fromIntegral canvasWidth)/2) (trY + (fromIntegral canvasHeight)/2))]
----                            # set SVG.fill "black"
----                            # set SVG.x "30"
----                            # set SVG.y "30"
----                            # set SVG.font_size "10px"
----                            -- # set SVG.stroke_width "2"
----                            # set SVG.stroke "black"
--                            # set SVG.x "30"
--                            # set SVG.y "50"
--                            # set SVG.fill "black"
--                    rect <- SVG.rect
--                        # set SVG.x "20"
--                        # set SVG.y "20"
--                        # set SVG.height "80"
--                        # set SVG.width "200"
--                        # set SVG.fill "none"
--                        # set SVG.stroke_width "1px"
--                        # set SVG.stroke "black"
--                        # set style [("background", "#b9a1a1ff")]
                    if (trX > -1 && trX <= (fromIntegral canvasWidth) &&
                          trY > -1 && trY <= (fromIntegral canvasHeight))
                            then do
                                case maybePosBox of
                                    Nothing -> add2Log "Position box is not found (1)"
                                    Just posBox -> element posBox # set style [("display", "none")]
                            else do
                                case maybePosBox of
                                    Nothing -> add2Log "Position box is not found (2)"
                                    Just posBox -> element posBox # set style [("display", "flex"),
                                                                               ("justify-content", "center")]
                                                                  # set children [headLine]

                -- Could try getElementById in Core
                -- add2Log :: String -> UI Element
                add2Log str = element logs #+ [UI.li # set html str]

                -- The triangle representing the turtle calculated by
                -- a Point (the center of the triangle) and
                -- a Direction (which determines which way the triangle faces) and using
                -- a Color to depict the the triangle
                primitiveTurtle :: UI Element
                primitiveTurtle = do
                    tr <- liftIO $ readIORef myTurtle
                    let pos = getPosition tr
                        dir = applyInt (getDirection tr) (*) 10
                    turtleInFrame
                    SVG.polygon
                            # set SVG.id "turtle"
                            # set SVG.fill (show $ getColor tr)
                            # set SVG.points (show (applyPoint pos (+) (applyInt dir (*) (-1))) ++ " " ++
                                              show (applyPoint pos (+) (applyInt (getDirection (rotateDir tr (40) )) (*) 10)) ++ " " ++
                                              show (applyPoint pos (+) (applyInt (getDirection (rotateDir tr (-40))) (*) 10)))
                            # set SVG.stroke_width "2"
                            # set SVG.stroke "black"

                -- I get the feeling that the triangle image might have multiple copies on the canvas
                draw :: Turtle -> UI Element
                draw oldTr = do
                    maybeTrE <- getElementById window "turtle"
                    case maybeTrE of
                        Nothing -> add2Log "No turtle id element"
                        Just turtleE -> do
                            newTr <- liftIO $ readIORef myTurtle
                            let
                                (Point newX newY) = getPosition newTr
                                newPos = (newX, newY)
                                (Point oldX oldY) = getPosition oldTr
                                oldPos = (oldX, oldY)

                            if ( oldPos == newPos ) ||  isPenUpBool newTr
                                then do
                                    Core.delete turtleE
                                    element canvas #+ [primitiveTurtle]
                                else do
                                    ln <- SVG.line
                                          # set SVG.stroke (show $ getColor oldTr)
                                          # set SVG.x1 (show oldX)
                                          # set SVG.y1 (show oldY)
                                          # set SVG.x2 (show newX)
                                          # set SVG.y2 (show newY)
                                    element canvas #+ [element ln]
                                    Core.delete turtleE
                                    element canvas #+ [primitiveTurtle]

                execCommand content tr =
                    case words (map toLower content) of
                        [] -> add2Log "Nothing was submitted\n"
                        ("home":_) ->
                            case runTurtle home undefined of
                                Just (action, newTr) -> do
                                    let homeTr = initTrtl newTr
                                    liftIO $ writeIORef myTurtle homeTr
                                    draw homeTr
                                    add2Log action
                                Nothing -> add2Log "Failed to return turtle to the middle!"
                        ("forward": value: _) ->
                            case str2Int value of
                                Just unit ->
                                    case runTurtle (forward unit) tr of
                                        Just (action, newTr) -> do
                                            liftIO $ writeIORef myTurtle newTr
                                            draw tr
                                            add2Log action
                                        Nothing -> add2Log "Failed to move forward\n"
                                Nothing ->
                                    case runTurtle (errorProtocol "Invalid number" tr) tr of
                                        Just (act, newTr) -> add2Log act
                                        Nothing -> add2Log "ErrorProtocol failed! (1)"
                        ("backward": value: _) ->
                            case str2Int value of
                                Just unit ->
                                    case runTurtle (backward unit) tr of
                                        Just (action, newTr) -> do
                                            liftIO $ writeIORef myTurtle newTr
                                            draw tr
                                            add2Log action
                                        Nothing -> add2Log "Failed to move turtle backward"
                                Nothing ->
                                    case runTurtle (errorProtocol "Invalid number" tr) tr of
                                        Just (act, newTr) -> add2Log act
                                        Nothing -> add2Log "ErrorProtocol failed! (2)"
                        ("left":value: _) ->
                            case str2Double value of
                                Just angle ->
                                    case runTurtle (left angle) tr of
                                        Just (action, newTr) -> do
                                            liftIO $ writeIORef myTurtle newTr
                                            draw tr
                                            add2Log action
                                        Nothing -> add2Log "Failed to turn turtle left\n"
                                Nothing ->
                                    case runTurtle (errorProtocol "Invalid angle" tr) tr of
                                        Just (act, newTr) -> do
                                            add2Log act
                                        Nothing -> add2Log "ErrorProtocol failed! (3)"
                        ("right": value: _) ->
                            case str2Double value of
                                Just angle ->
                                    case runTurtle (right angle) tr of
                                        Just (action, newTr) -> do
                                            liftIO $ writeIORef myTurtle newTr
                                            draw tr
                                            add2Log action
                                        Nothing -> add2Log "Failed to turn turtle right\n"
                                Nothing ->
                                    case runTurtle (errorProtocol "Invalid angle" tr) tr of
                                        Just (act, newTr) -> add2Log act
                                        Nothing -> add2Log "ErrorProtocol failed! (4)"
                        ("pen": value: _) ->
                            case value of
                                "up" ->
                                    case runTurtle penUp tr of
                                        Just (action, newTr) -> do
                                            liftIO $ writeIORef myTurtle newTr
                                            add2Log action
                                        Nothing -> add2Log "Failed to lift pen up\n"
                                "down" ->
                                    case runTurtle penDown tr of
                                        Just (action, newTr) -> do
                                            liftIO $ writeIORef myTurtle newTr
                                            add2Log action
                                        Nothing -> add2Log "Failed to put pen down\n"
                                _ ->
                                    case runTurtle (errorProtocol "Invalid pen status" tr) tr of
                                        Just (act, newTr) -> add2Log act
                                        Nothing -> add2Log "ErrorProtocol failed! (5)"
                        ("set": value: color) ->
                            case color of
                                [colorWord] -> 
                                    case str2Color $ trim colorWord of
                                        Nothing ->  add2Log ("Color \"" ++ trim colorWord ++ "\" is unrecognized.")
                                        Just clr ->
                                            case runTurtle (setColor clr) tr of
                                                Nothing -> add2Log "Colour update failed (1)\n"
                                                Just (action, newTr) -> do
                                                    liftIO $ writeIORef myTurtle newTr
                                                    draw tr
                                                    add2Log action
                                [r, g, b, a] ->
                                    case traverse str2Int [r, g, b] of
                                        Nothing -> add2Log "One or more of the numbers for colour components is/are invalid\n"
                                        Just [ri, gi, bi] -> do
                                            case str2Double a of
                                                Nothing -> add2Log "Alpha color component is invalid"
                                                Just ad -> do
                                                    let af = double2Float ad
                                                    if (ri > 256) || (ri < 0) ||
                                                       (gi > 256) || (gi < 0) ||
                                                       (bi > 256) || (bi < 0) ||
                                                       (af > 1) || (af < 0)
                                                        then
                                                            case runTurtle (errorProtocol "One or more arguments are out of bound for color" tr) tr of
                                                                Just (act, newTr) -> do
                                                                    add2Log act
                                                                Nothing -> add2Log "ErrorProtocol failed! (6)"
                                                        else
                                                            case runTurtle (setColor (createColor ri gi bi af)) tr of
                                                                Just (action, newTr) -> do
                                                                    liftIO $ writeIORef myTurtle newTr
                                                                    draw tr
                                                                    add2Log action
                                                                Nothing -> add2Log "Color update failed (2)\n"
                                _ -> add2Log "Inadequate amount of parameters for setting the color"
                        _ -> case runTurtle (errorProtocol "Invalid command" tr) tr of
                            Just (act, newTr) -> do
                               add2Log act
                            Nothing -> add2Log "ErrorProtocol failed! (7)"
            trPosBox <- UI.div
                        # set UI.id_ "posBox"
                        # set style [("display", "none")]
            element canvas #+ [primitiveTurtle]

            -- Might have mixed up column and row
            getBody window #+
                [
                    UI.h2 #+ [string header]
                          # set style [("margin", "1rem"),
                                       ("display", "flex"),
                                       ("justify-content", "center"),
                                       ("align-content", "center")],
                    UI.h4 #+ [string intro]
                          # set style [("max-width", "800"),
                                       ("margin", "1rem")],


                    UI.div #+ [element canvas]
                           # set style [("display", "flex"),
                                        ("justify-content", "center")],

                    UI.div #+ [element fractalSelect]
                           # set style [("display", "flex"),
                                        ("justify-content", "center"),
                                        ("column-gap", "0.2rem"),
                                        ("margin", "0.2rem")],
                    UI.div # set UI.id_ "fractalValues"
                           #+ [string "Angle: ",
                               element fractalAngle,
                               string "Movement amount:",
                               element fractalUnit,
                               string "Iteration:",
                               element fractalIteration]
                           # set style [("display", "none"),
                                        ("justify-content", "center"),
                                        ("column-gap", "0.2rem"),
                                        ("margin", "0.2rem")],
                    UI.div #+ [element command,
                               element execute,
                               element clear,
                               element restart]
                           # set style [("display", "flex"),
                                        ("justify-content", "center"),
                                        ("column-gap", "0.2rem"),
                                        ("margin", "0.2rem")],
                    element showBox,
                    element trPosBox,
                    element help,
                    UI.div #+ [element logs]
                           # set scrolling "bottom"
                
                ]

            on UI.selectionChange fractalSelect $ \selected ->
                case selected of
                    Nothing -> add2Log "Noinput type is selected"
                    Just 0 -> do
                        element command # set style [("display", "")]
                        tmpElement <- getElementById window "fractalValues"
                        case tmpElement of
                            Nothing -> add2Log "Error occured with getting the fractal value elements"
                            Just divElement -> 
                                element divElement # set style [("display", "none")]
--                        element fractalAngle 
--                        element fractalUnit # set style [("display", "none")]
--                        element fractalIteration # set style [("display", "none")]
                    Just val -> do
                        element command # set style [("display", "none")]
                        tmpElement <- getElementById window "fractalValues" 
                        case tmpElement of
                            Nothing -> add2Log "Error occured with getting the fractal value elements"
                            Just divElement -> 
                                element divElement # set style [("display", "flex")]
--                        element fractalAngle # set style [("display", "")]
--                        element fractalUnit # set style [("display", "")]
--                        element fractalIteration # set style [("display", "")]
                        case val of
                            1 -> do
                                element fractalAngle # set value "60"
                                element fractalUnit # set value "10"
                                element fractalIteration # set value "4"
                                element fractalAxiom # set value "F--F--F"
                            2 -> do
                                element fractalAngle # set value "90"
                                element fractalUnit # set value "5"
                                element fractalIteration # set value "10"
                                element fractalAxiom # set value "F"
                            3 -> do
                                element fractalAngle # set value "120"
                                element fractalUnit # set value "10"
                                element fractalIteration # set value "5"
                                element fractalAxiom # set value "F-G-G"
                            4 -> do
                                element fractalAngle # set value "25"
                                element fractalUnit # set value "10"
                                element fractalIteration # set value "4"
                                element fractalAxiom # set value "X"

            on UI.click clear $ const $ do
                element canvas # set children []
                               #+ [primitiveTurtle]

            on UI.click restart $ const $ do
                execCommand "home" undefined
                element canvas # set children []
                               #+ [primitiveTurtle]

            on UI.checkedChange logbox $ \box ->
                case box of
                    True -> element logs # set style [("display", "")]
                    False -> element logs # set style [("display","none")]

            on UI.checkedChange helpbox $ \box ->
                case box of
                    True -> element help # set style [("display","")]
                    False -> element help # set style [("display", "none")]

            on UI.click execute $ const $ do
                choice <- fractalSelect # Core.get selection
                tr <- liftIO (readIORef myTurtle)
                case choice of
                    Just 0 -> do
                        content <- currentValue commandIn
                        element command # set value ""
                        execCommand content tr
                    Just val -> do
                        degree <- fractalAngle # Core.get value
                        case str2Double degree of
                            Nothing -> add2Log "Couldn't read the angle"
                            Just angle -> do
                                step <- fractalUnit # Core.get value
                                case str2Int step of
                                    Nothing -> add2Log "Couldn't read the step unit"
                                    Just unit -> do
                                        iteration <- fractalIteration # Core.get value
                                        case str2Int iteration of
                                            Nothing -> add2Log "Couldn't read the iteration amount"
                                            Just i -> do 
                                                axiom <- fractalAxiom # Core.get value
                                                case val of
                                                    1 -> do
                                                        let fract = Fractal {rules = [('F', "F+F--F+F")], degrees = angle, step = unit}
                                                            turtles = lSystem fract axiom i tr
                                                        list <- mapM (\newTr -> 
                                                                    do
                                                                        oldTr <- liftIO $ readIORef myTurtle
                                                                        liftIO $ writeIORef myTurtle newTr
                                                                        draw oldTr
                                                                        ) turtles
                                                        add2Log "Koch Snowflake is ready"
                                                    2 -> do
                                                        let fract = Fractal {rules = [('F', "F+G"), ('G', "F-G")], degrees = angle, step = unit}
                                                            turtles = lSystem fract axiom i tr
                                                        list <- mapM (\newTr -> 
                                                                    do
                                                                        oldTr <- liftIO $ readIORef myTurtle
                                                                        liftIO $ writeIORef myTurtle newTr
                                                                        draw oldTr
                                                                        ) turtles
                                                        add2Log "Dragon Curve is ready"
                                                    3 -> do
                                                        let fract = Fractal {rules = [('F', "F-G+F+G-F"), ('G', "GG")], degrees = angle, step = unit}
                                                            turtles = lSystem fract axiom i tr
                                                        list <- mapM (\newTr -> 
                                                                    do
                                                                        oldTr <- liftIO $ readIORef myTurtle
                                                                        liftIO $ writeIORef myTurtle newTr
                                                                        draw oldTr
                                                                        ) turtles
                                                        add2Log "Sierpinski Triangle is ready"
                                                    4 -> do
                                                        let fract = Fractal {rules = [('F', "FF"), ('X', "F+[[X]-X]-F[-FX]+X")], degrees = angle, step = unit}
                                                            turtles = lSystem fract axiom i tr
                                                        list <- mapM (\newTr -> 
                                                                    do
                                                                        oldTr <- liftIO $ readIORef myTurtle
                                                                        liftIO $ writeIORef myTurtle newTr
                                                                        draw oldTr
                                                                        ) turtles
                                                        add2Log "Plant growth fractal is ready"



{-
    on UI.sendValue command $ (. trim) $ \content -> do
        element command # set value ""
                        # set placeholder "Write commands here"
        when (not (null content)) (takeCommand content)
-}

str2Int :: String -> Maybe Int
str2Int s = readMaybe s

str2Double :: String -> Maybe Double
str2Double s = readMaybe s
