import Distribution.Simple (ProfDetailLevel(ProfDetailAllFunctions))
-- Task 1
f1 :: [IO a] -> IO a
f1 [] = error "Empty list of IO actions"
f1 [x] = x
f1 (x : xs) = do
    x
    f1 xs

main = do
    while (ioBool read10 "exit012345")


-- Task 4
while :: IO Bool -> IO ()
while x = do
    b <- x 
    if b 
        then while x
        else putStrLn "While loop finished"

-- Task 5
read10 :: IO String
read10 = f2 $ take 10 actions where
    actions = getChar : actions

f2 :: [IO a] -> IO [a]
f2 [] = error "Empty list"
f2 [x] = x >>= \a -> return [a]
f2 (x:xs) = x >>= \a -> f2 xs >>= \as -> return (a:as)

ioBool :: IO String -> String -> IO Bool
ioBool ioStr str = ioStr >>= \strIo -> putStr (show (strIo /= str)) >> return (strIo /= str)
