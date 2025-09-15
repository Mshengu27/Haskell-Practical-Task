-- HC1T2 - Task 2: Pure Function Example

-- Pure function to calculate the area of a circle
circleArea :: Floating a => a -> a
circleArea r = pi * r ^ 2

-- Main to test HC1T2
main :: IO ()
main = do
    putStrLn "HC1T2 - Pure Function Example"
    putStrLn $ "circleArea with radius 3 = " ++ show (circleArea 3)
    putStrLn $ "circleArea with radius 5 = " ++ show (circleArea 5)
