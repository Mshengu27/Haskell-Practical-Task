-- HC1T3 - Task 3: Checking if a Number is Greater than 18

-- Function to check if a number is greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- Main to test HC1T3
main :: IO ()
main = do
    putStrLn "HC1T3 - Checking if a Number is Greater than 18"
    putStrLn $ "greaterThan18 20 = " ++ show (greaterThan18 20)
    putStrLn $ "greaterThan18 15 = " ++ show (greaterThan18 15)
    putStrLn $ "greaterThan18 18 = " ++ show (greaterThan18 18)
