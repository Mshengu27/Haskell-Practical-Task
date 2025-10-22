module Main where

import Control.Exception
import Data.Char (toLower)
import Data.List (sort)

-- HC12T1: Print a Welcome Message
welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers a b = a + b

-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

firstTenFibonacci :: [Int]
firstTenFibonacci = map fibonacci [0..9]

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str =
  let clean = map toLower (filter (/= ' ') str)
  in clean == reverse clean

-- HC12T6: Sort a List of Integers
sortList :: [Int] -> [Int]
sortList = sort

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y     = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- HC12T9: Read and Print File Content
readFileSafe :: FilePath -> IO ()
readFileSafe path = do
  result <- try (readFile path) :: IO (Either IOError String)
  case result of
    Left _ -> putStrLn "Error: File not found or cannot be opened."
    Right content -> putStrLn ("File content:\n" ++ content)

-- HC12T10: Mathematical Operations Module (Simple inline example)
add, subtractNum, multiply :: Int -> Int -> Int
add a b = a + b
subtractNum a b = a - b
multiply a b = a * b

-- Main Menu
main :: IO ()
main = do
  putStrLn "Choose a task (1-10):"
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "--- HC12T1: Print a Welcome Message ---"
      welcomeMessage

    "2" -> do
      putStrLn "--- HC12T2: Add Two Numbers ---"
      putStrLn "Enter first number:"
      a <- readLn
      putStrLn "Enter second number:"
      b <- readLn
      putStrLn ("Sum: " ++ show (addTwoNumbers a b))

    "3" -> do
      putStrLn "--- HC12T3: Factorial Function ---"
      putStrLn "Enter a positive integer:"
      n <- readLn
      putStrLn ("Factorial: " ++ show (factorial n))

    "4" -> do
      putStrLn "--- HC12T4: First 10 Fibonacci Numbers ---"
      print firstTenFibonacci

    "5" -> do
      putStrLn "--- HC12T5: Palindrome Checker ---"
      putStrLn "Enter a word or phrase:"
      str <- getLine
      if isPalindrome str
        then putStrLn "It is a palindrome!"
        else putStrLn "It is not a palindrome."

    "6" -> do
      putStrLn "--- HC12T6: Sort a List of Integers ---"
      putStrLn "Enter integers separated by spaces:"
      input <- getLine
      let nums = map read (words input) :: [Int]
      putStrLn ("Sorted list: " ++ show (sortList nums))

    "7" -> do
      putStrLn "--- HC12T7: Calculate Circle Area ---"
      putStrLn "Enter the radius:"
      r <- readLn
      putStrLn ("Area: " ++ show (calculateCircleArea r))

    "8" -> do
      putStrLn "--- HC12T8: Merge Two Sorted Lists ---"
      putStrLn "Enter first sorted list of integers (e.g. 1 3 5):"
      list1 <- fmap (map (read :: String -> Int) . words) getLine
      putStrLn "Enter second sorted list of integers (e.g. 2 4 6):"
      list2 <- fmap (map (read :: String -> Int) . words) getLine
      putStrLn ("Merged list: " ++ show (mergeLists list1 list2))

    "9" -> do
      putStrLn "--- HC12T9: Read and Print File Content ---"
      putStrLn "Enter file name:"
      fileName <- getLine
      readFileSafe fileName

    "10" -> do
      putStrLn "--- HC12T10: Mathematical Operations Module ---"
      putStrLn ("Add 5 and 3: " ++ show (add 5 3))
      putStrLn ("Subtract 10 - 4: " ++ show (subtractNum 10 4))
      putStrLn ("Multiply 6 * 7: " ++ show (multiply 6 7))

    _ -> putStrLn "Invalid choice. Please enter a number between 1 and 10."
