newtype State s a = State (s -> (a, s))

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\s -> (a,s))
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) sf sx = State (\s0 -> 
        let (f, s1) = runState sf s0
            (x, s2) = runState sx s1
        in (f x, s2))

instance Monad (State s) where
    st >>= f = State (\s -> let (a,s') = runState st s in runState (f a) s')

liftM :: Monad t => (a -> b) -> t a -> t b
liftM f m = do
    x <- m
    return (f x) -- return always expects one value

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
    print (runState (countLetters "Hello") 6)