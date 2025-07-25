import Data.Map
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromMaybe)
import Control.Monad (liftM)

newtype State s a = State (s -> (a, s))

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure a = State (\s -> (a, s))
    sf <*> sx = State (\s0 ->
        let (f, s1) = runState sf s0
            (x, s2) = runState sx s1
        in (f x, s2))   

instance Monad (State s) where
    st >>= f = State (\s ->
        let (a, s') = runState st s in runState (f a) s')

runState :: State s a -> s -> (a, s)
runState (State f) = f

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put newS = State (\_ -> ((), newS))

type Inventory = Map String Int

addItems :: String -> Int -> State Inventory String
addItems item amount = do
    inventory <- get

    case Map.lookup item inventory of
        Nothing -> do
            put (Map.insert item amount inventory)
            return ("New item added to inventory: " ++ show amount ++ " " ++ item)
        Just stock -> do
            put $ Map.insert item (stock + amount) inventory
            return ("Inventory entry of " ++ item ++ " updated, the stock is at: " ++ show (Map.lookup item inventory))

sellItems :: String -> Int -> State Inventory String
sellItems item amount = do
    inventory <- get

    case Map.lookup item inventory of
        Nothing -> return (item ++ " is/are not sold here\n")
        Just stock -> do
            if stock > amount
                then do
                    put $ Map.insert item (stock-amount) inventory
                    return ("Sold")
                else
                    return ("There is not enough " ++ item)