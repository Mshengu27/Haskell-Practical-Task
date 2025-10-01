-- Haskell Chapter 6 Practical Tasks
-- Recursion and List Processing

import Data.Char (digitToInt)
import Data.Char (toUpper)

-- HC6T1: Factorial (Recursive)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- HC6T2: Fibonacci (Recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


-- HC6T3: Sum of Elements Using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0


-- HC6T4: Product of Elements Using foldl
productList :: [Int] -> Int
productList = foldl (*) 1


-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]


-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
  | y == x    = True
  | otherwise = elementExists y xs


-- HC6T7: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs


-- HC6T8: Filter Even Numbers
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
  | even x    = x : filterEvens xs
  | otherwise = filterEvens xs


-- HC6T9: Map Implementation
mapCustom :: (a -> b) -> [a] -> [b]
mapCustom _ [] = []
mapCustom f (x:xs) = f x : mapCustom f xs


-- HC6T10: Digits of a Number (Recursive)
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]


-- Main function to test all tasks
main :: IO ()
main = do
  putStrLn "--- HC6T1: Factorial ---"
  print (factorial 0)   -- 1
  print (factorial 5)   -- 120

  putStrLn "\n--- HC6T2: Fibonacci ---"
  print (fibonacci 0)   -- 0
  print (fibonacci 7)   -- 13

  putStrLn "\n--- HC6T3: Sum of List ---"
  print (sumList [1..5])   -- 15

  putStrLn "\n--- HC6T4: Product of List ---"
  print (productList [1..5]) -- 120

  putStrLn "\n--- HC6T5: Reverse List ---"
  print (reverseList [1,2,3,4]) -- [4,3,2,1]

  putStrLn "\n--- HC6T6: Element Exists ---"
  print (elementExists 3 [1,2,3,4]) -- True
  print (elementExists 9 [1,2,3,4]) -- False

  putStrLn "\n--- HC6T7: List Length ---"
  print (listLength [1,2,3,4,5]) -- 5

  putStrLn "\n--- HC6T8: Filter Evens ---"
  print (filterEvens [1..10]) -- [2,4,6,8,10]

  putStrLn "\n--- HC6T9: Map Implementation ---"
  print (mapCustom (+1) [1,2,3]) -- [2,3,4]
  print (mapCustom (*2) [1,2,3]) -- [2,4,6]

  putStrLn "\n--- HC6T10: Digits of a Number ---"
  print (digits 12345) -- [1,2,3,4,5]
  print (digits 7)     -- [7]
