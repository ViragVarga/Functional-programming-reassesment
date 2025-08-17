import Control.Monad.Trans.State
import Control.Monad.Trans

increment :: Int -> StateT Int Maybe String
increment n = do
    current <- get
    if even n
        then do
            put (current + 1)
            return "Success"
        else
            lift Nothing

{-| n % 2 == 0 = StateT (\s -> Maybe ("Success", s + 1))
            | otherwise = StateT (\s -> Maybe ("State unchanged", s))
            -}