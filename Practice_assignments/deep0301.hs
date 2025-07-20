import Prelude hiding (Left, Right, Either)

data Either a = Right a | Left a

instance Monad Either where
    (>>=) :: Either a -> (a -> Either b) -> Either b
    Left a >>= _ = Left b
    Right x >>= f = f x

    return :: a -> Either a
    return = Right
