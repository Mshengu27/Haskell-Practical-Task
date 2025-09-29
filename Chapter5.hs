-- HC5T1: Using applyTwice
-- Define a function that takes a function and an integer,
-- then applies the function three times.
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))


-- HC5T2: Filtering Odd Numbers
-- Extract all odd numbers from 1 to 30.
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]


-- HC5T3: Checking for Uppercase Letters
-- Check if any word in a list starts with an uppercase letter.
import Data.Char (isUpper)

hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\word -> not (null word) && isUpper (head word))


-- HC5T4: Using Lambda Functions
-- Rewrite biggerThan10 with a lambda.
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10


-- HC5T5: Partial Application
-- Multiply any number by 5 using partial application.
multiplyByFive :: Int -> Int
multiplyByFive = (* 5)


-- HC5T6: Function Composition
-- Square numbers and filter only the even ones.
squareEvens :: [Int] -> [Int]
squareEvens = filter even . map (^2)


-- HC5T7: The $ Operator
-- Rewrite result using $ to reduce parentheses.
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]


-- HC5T8: Point-Free Style
-- Convert addFive into point-free style.
addFive :: Int -> Int
addFive = (+5)


-- HC5T9: Higher-Order Function to Transform a List
-- Apply a function twice to every element of a list.
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)


-- HC5T10: Combining Higher-Order Functions
-- Check if any squared value in a list is greater than 50.
hasSquareGreaterThan50 :: [Int] -> Bool
hasSquareGreaterThan50 = any (>50) . map (^2)


-- Main function to test ALL tasks
main :: IO ()
main = do
  putStrLn "--- HC5T1: applyThrice ---"
  print (applyThrice (+1) 5)    -- 8
  print (applyThrice (*2) 2)    -- 16

  putStrLn "\n--- HC5T2: oddNumbers ---"
  print oddNumbers              -- [1,3,5,...,29]

  putStrLn "\n--- HC5T3: hasUppercaseWord ---"
  print (hasUppercaseWord ["hello", "World"]) -- True
  print (hasUppercaseWord ["all", "lower"])   -- False

  putStrLn "\n--- HC5T4: biggerThan10 ---"
  print (biggerThan10 5)   -- False
  print (biggerThan10 15)  -- True

  putStrLn "\n--- HC5T5: multiplyByFive ---"
  print (multiplyByFive 3)   -- 15
  print (multiplyByFive 10)  -- 50

  putStrLn "\n--- HC5T6: squareEvens ---"
  print (squareEvens [1..10])  -- [4,16,36,64,100]

  putStrLn "\n--- HC5T7: result ---"
  print result  -- 84

  putStrLn "\n--- HC5T8: addFive ---"
  print (addFive 7)  -- 12

  putStrLn "\n--- HC5T9: transformList ---"
  print (transformList (+1) [1,2,3])  -- [3,4,5]
  print (transformList (*2) [1,2,3])  -- [4,8,12]

  putStrLn "\n--- HC5T10: hasSquareGreaterThan50 ---"
  print (hasSquareGreaterThan50 [1,2,3,7])   -- True (7^2=49 → False, but 8^2 would be True)
  print (hasSquareGreaterThan50 [1,2,3,4,5]) -- True (5^2=25 → False, but 8+ would give True)
