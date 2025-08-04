newtype Expr = Expr { eval :: Int}

constant :: Int -> Expr
constant n = Expr n

add :: Expr -> Expr -> Expr
add e1 e2 = Expr (eval e1 + eval e2)
