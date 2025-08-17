{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
module Draw_2(
    startDrawingSess
) where

import Text.Read
import GHC.Float (int2Double, double2Float)
import GHC.Unicode (toLower)
 
import Data.IORef
import Data.List.Extra
 
import Graphics.UI.Threepenny as UI hiding (Point, map)
import Graphics.UI.Threepenny.Core as Core
import qualified Graphics.UI.Threepenny.SVG as SVG

import Turtle (Point(..), Color(..), Turtle(..), runTurtle, getPosition,
               getDirection, rotateDir, getColor, home, forward, applyInt, 
               backward, left, right, penUp, penDown, isPenUpBool, applyPoint,
               str2Color, setColor, setPosition, createColor, errorProtocol)

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
    restart <- UI.button #+ [string "Restart"]
    clear <- UI.button #+ [string "Clear"]
    logs <- UI.ul # set style [("display", "none"),
                               ("justify-content", "left")]
                  #+ [string "Actions taken:"]
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
    let
        initTrtl tr = setPosition tr (Point (int2Double canvasWidth/2) (int2Double canvasHeight/2))
    case runTurtle home undefined of
        Nothing -> debug "Failed to generate initial turtle state\n"
        Just (initStr, tr) -> do
            myTurtle <- liftIO $ newIORef $ initTrtl tr

            let header = "Welcome to my Turtle Game!"
                intro = "You can already see our simple Turtle on the canvas, facing forward. " ++
                        "To make the little triangle move, you need to give commands. " ++
                        "For help with the commands you can check the box below and the acceptable commands appear.\n"

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
                    SVG.polygon
                        # set SVG.id "turtle"
                        # set SVG.fill (show $ getColor tr)
                        # set SVG.points (show (applyPoint pos (+) (applyInt dir (*) (-1))) ++ " " ++
                                          show (applyPoint pos (+) (applyInt (getDirection (rotateDir tr (40) )) (*) 10)) ++ " " ++
                                          show (applyPoint pos (+) (applyInt (getDirection (rotateDir tr (-40))) (*) 10)))
                        # set SVG.stroke_width "2"
                        # set SVG.stroke "black"

            canvas <- SVG.svg
                # set SVG.width (show canvasWidth)
                # set SVG.height (show canvasHeight)
                # set style [("border", "solid black 1px"), 
                             ("background", "#d1d1d1ff"), 
                             ("margin", "auto 74px")]
                #+ [primitiveTurtle]

            -- Might have mixed up column and row
            getBody window #+
                [
                    column
                    [
                        UI.h2 #+ [string header]
                              # set style [("margin", "1rem"),
                                           ("display", "flex"),
                                           ("justify-content", "center"),
                                           ("align-content", "center")],
                        UI.h4 #+ [string intro]
                              # set style [("max-width", "800"),
                                           ("margin", "1rem")],
                              
                              
                        element canvas,
                        UI.div #+ [element command,
                                   element execute,
                                   element clear,
                                   element restart]
                               # set style [("display", "flex"),
                                            ("justify-content", "center"),
                                            ("column-gap", "0.2rem"),
                                            ("margin", "0.2rem")],
                        element showBox,
                        element help,
                        element logs
                    ]
                ]

            on UI.click clear $ const $ do
                primTr <- primitiveTurtle
                element canvas # set children [primTr]
            
--            on UI.click restart $ const $ do
--                primTr <- primitiveTurtle

            on UI.checkedChange logbox $ \box ->
                case box of
                    True -> element logs # set style [("display", "")]
                    False -> element logs # set style [("display","none")]

            on UI.checkedChange helpbox $ \box ->
                case box of
                    True -> element help # set style [("display","")]
                    False -> element help # set style [("display", "none")]

            on UI.click execute $ const $ do
                let
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

                                if ( oldPos == newPos ) ||  isPenUpBool oldTr
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

                content <- currentValue commandIn
                element command # set value ""
                tr <- liftIO (readIORef myTurtle)
                case words (map toLower content) of
                    [str] ->
                        case str of
                            "home" ->
                                case runTurtle home undefined of
                                    Just (action, newTr) -> do
                                        let homeTr = initTrtl newTr
                                        liftIO $ writeIORef myTurtle homeTr
                                        draw homeTr
                                        add2Log action
                                    Nothing -> add2Log "Failed to return turtle to the middle!"
                            _ ->
                                case runTurtle (errorProtocol "The given command is unknown!" tr) tr of
                                    Just (act, _) -> add2Log act
                                    Nothing -> add2Log "ErrorProtocol failed! (1)"
                    [] -> add2Log "Nothing was submitted\n"
                    _ ->
                        case words content of
                            ["forward", value] ->
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
                                            Nothing -> add2Log "ErrorProtocol failed! (2)"
                            ["backward", value] ->
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
                                            Nothing -> add2Log "ErrorProtocol failed! (3)"
                            ["left", value] ->
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
                                            Nothing -> add2Log "ErrorProtocol failed! (4)"
                            ["right", value] ->
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
                                            Nothing -> add2Log "ErrorProtocol failed! (5)"
                            ["pen", value] ->
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
                                            Nothing -> add2Log "ErrorProtocol failed! (6)"
                            -- It might gonna read the length as overall characters in the whole list e.g. ["33", "16", "170", "0.63"] -> 11
                            ("set":value) ->
                                case value of
                                    [_, color] ->
                                        case str2Color $ trim color of
                                            Nothing ->  add2Log ("Color \"" ++ trim color ++ "\" is unrecognized.")
                                            Just clr -> 
                                                case runTurtle (setColor clr) tr of
                                                    Nothing -> add2Log "Colour update failed (1)\n"
                                                    Just (action, newTr) -> do
                                                        liftIO $ writeIORef myTurtle newTr
                                                        draw tr
                                                        add2Log action
                                    [_, r, g, b, a] ->
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
                                                                    Nothing -> add2Log "ErrorProtocol failed! (7)"
                                                            else
                                                                case runTurtle (setColor (createColor ri gi bi af)) tr of
                                                                    Just (action, newTr) -> do
                                                                        liftIO $ writeIORef myTurtle newTr
                                                                        draw tr
                                                                        add2Log action
                                                                    Nothing -> add2Log "Color update failed (2)\n"

                                    _ -> case runTurtle (errorProtocol "Too many or too few arguments for setting a color" tr) tr of
                                            Just (act, newTr) -> do
                                               add2Log act
                                            Nothing -> add2Log "ErrorProtocol failed! (8)"
                            _ -> case runTurtle (errorProtocol "Invalid command" tr) tr of
                                    Just (act, newTr) -> do
                                       add2Log act
                                    Nothing -> add2Log "ErrorProtocol failed! (9)"

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
