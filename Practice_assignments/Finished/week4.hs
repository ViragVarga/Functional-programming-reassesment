{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
newtype Writer str a = Writer ([str] -> ([str], a))

instance Functor (Writer w) where
    fmap = liftM

instance Applicative (Writer w) where
    pure a = Writer (\w -> (w, a))
    wf <*> wx = Writer (\w0 ->
                    let (w1, f) = retrieve wf w0
                        (w2, x) = retrieve wx w1
                    in (w2, f x))

instance Monad (Writer w) where
    w >>= f = Writer (\w0 -> let (w2, a) = retrieve w w0 in retrieve (f a) w2)

liftM :: Monad t => (a -> b) -> t a -> t b
liftM f a = do
    x <- a
    return (f x)

retrieve :: Writer w a -> [w] -> ([w], a)
retrieve (Writer f) = f 

tell :: a -> Writer a ()
tell a = Writer $ \w -> let newW = (w ++ [a]) in ( newW, () )

main = do
    let (log, result) = retrieve testWriter []
    putStrLn $ "Log: " ++ show log
    putStrLn $ "Result: " ++ show result

testWriter :: Writer String Int
testWriter = do
    (<*>) (tell [] >> pure (+1)) (tell [] >> pure 2)

showWriter :: (Show w, Show a) => Writer w a -> String
showWriter writer =
    let (ws, a) = retrieve writer []
        str = printable ws
        i = show a
    in "[" ++ str ++ ", " ++ i ++ "]"


printable :: Show a => [a] -> String
printable [] = ""
printable (x:xs) = show x ++ printable xs