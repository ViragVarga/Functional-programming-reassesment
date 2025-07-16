readTwoChars :: IO String
readTwoChars = getChar >>= (\char1 -> getChar >>= (\char2 -> return [char1, char2]))

readTwoCharsDo :: IO String
readTwoCharsDo = do
    char1 <- getChar
    char2 <- getChar
    return [char1, char2]

main :: IO ()
main = do 
    str <- readTwoChars
    putStrLn $ "Your string: " ++ str ++ "\nNow with do notation:"
    str <- readTwoCharsDo
    putStrLn $ "Your \"do\" string: " ++ str