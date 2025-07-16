{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

import Data.Char (isDigit)
isNumber :: String -> Bool
isNumber [] = True
isNumber (x:xs) = isDigit x && isNumber xs


getAge :: String -> Integer
getAge [] = -1
getAge a
    | isNumber a = read a
    | otherwise = -1

main :: IO ()
main = do
    userInput <- getLine
    let age = getAge userInput
    if age > 0 
        then putStrLn("You'll be 100 in " ++ show (100-age) ++ " years!")
        else do
            putStrLn "Not acceptable number. Try again!"
            main