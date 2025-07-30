import Control.Monad (forM_)
type Time = Double
newtype Signal a = Signal {(@) :: Time -> a}

instance Functor Signal where
    fmap = mapS

instance Applicative Signal where
    pure = constant
    (<*>) = applyS


constant :: a -> Signal a
constant x = Signal $ const x

timeS :: Signal Time
timeS = Signal id

mapS :: (a -> b) -> Signal a -> Signal b
mapS f sx = pure f <*> sx

applyS :: Signal (a -> b) -> Signal a -> Signal b
applyS sf sx = Signal(\t -> (sf @ t) (sx @ t))

sinS :: Signal Double
sinS = fmap sin timeS

discretize :: Signal Double -> Signal Int
discretize = fmap round

toBars :: Signal Int -> Signal String
toBars = fmap (`replicate` '#')

display :: Signal String -> IO ()
display ss = forM_ [0, 0.05 .. 100] (\x -> putStrLn (ss @ x))

scale :: Signal Double -> Signal Double
scale sx = Signal (\t ->
            let x = (sx @ t) in 30 * (x + 2))

run :: IO ()
run = display . toBars . discretize . scale $ sinS

main = run