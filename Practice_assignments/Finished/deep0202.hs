safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

num1 :: Double
num1 = 100
num2 :: Double
num2 = 4
num3 :: Double
num3 = 0

main :: IO ()
main = do
    let result = safeDivide num1 num2 >>= ( `safeDivide` num3)
    case result of
        Just x -> putStrLn ("Result: " ++ show x)
        Nothing -> putStrLn "Error: Division by 0"

-- (100 / 5 ) / 2 => 20 / 2 => 10