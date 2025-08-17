-- Part 1
data List a = Nil | Cons a (List a)

instance Functor (List a) where
    fmap = liftM

instance Applicative (List a) where
    pure :: a -> List a
    pure a = List (Const a (Nil))

    (<*>) :: List (a -> b) -> List a -> List b
    lf <*> lx = do
        let f = runList lf
            x = runList lx
        in (f x)

instance Monad List where
    return
    >>=

liftM :: Monad t => (a -> b) -> t a -> t b
liftM f a = a >>= \x -> return (f x)

runList :: List a -> a -> List b
runList f = f a