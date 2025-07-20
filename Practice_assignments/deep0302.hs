newtype State s a = State (s -> (a, s))

instance Monad (State s) where
    return x = State (\s -> (x,s))
    st >>= f = State (\s -> let (a,s') = runState st s in runState (f a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))


countLetters :: String -> State Int Int
countLetters [] = return 0
countLetters (x:xs) = do
    modify (+1)
    restCount <- countLetters xs
    return (1 + restCount)


runState :: State s a -> s -> (a, s)
runState (State f) = f

main = do
    print (runState (countLetters "Hello") 0)