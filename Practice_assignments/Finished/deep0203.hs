class Printable a where
    printMe :: a -> String

instance Printable Int where
    printMe = show

instance Printable Bool where
    printMe True = "True"
    printMe False = "False"

printList :: Printable a => [a] -> String
printList [] = ""
printList (x:xs) = printMe x ++ "\t" ++ printList xs

btw15 :: (Ord a, Num a) => [a] -> [Bool]
btw15 [] = []
btw15 (x:xs)
    | x > 1 && x < 5 = True : btw15 xs
    | otherwise = False : btw15 xs


test :: [Int]
test = [3 ,1 ,5 , 7, 0, 4]
main = do
    let new = btw15 test
    putStrLn $ "Original List:\n" ++
                printList test ++
                "\nNumbers between 1-5:\n" ++
                printList new
