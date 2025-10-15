import System.IO (hFlush, stdout)
import Data.Char (toUpper)

-- HC11T1: Greet the User
hc11t1 :: IO ()
hc11t1 = do
  putStrLn "--- HC11T1: Greet the User ---"
  putStr "Enter your name: "
  hFlush stdout
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

-- HC11T2: Count Characters in a Line
hc11t2 :: IO ()
hc11t2 = do
  putStrLn "--- HC11T2: Count Characters in a Line ---"
  putStr "Enter a line: "
  hFlush stdout
  line <- getLine
  putStrLn ("Number of characters: " ++ show (length line))

-- HC11T3: Double a Number
hc11t3 :: IO ()
hc11t3 = do
  putStrLn "--- HC11T3: Double a Number ---"
  putStr "Enter a number: "
  hFlush stdout
  input <- getLine
  let n = read input :: Int
  putStrLn ("Double: " ++ show (n * 2))

-- HC11T4: Concatenate Two Lines
hc11t4 :: IO ()
hc11t4 = do
  putStrLn "--- HC11T4: Concatenate Two Lines ---"
  putStr "Enter first line: "
  hFlush stdout
  line1 <- getLine
  putStr "Enter second line: "
  hFlush stdout
  line2 <- getLine
  putStrLn ("Concatenated: " ++ line1 ++ line2)

-- HC11T5: Repeat Until "quit"
hc11t5 :: IO ()
hc11t5 = do
  putStrLn "--- HC11T5: Repeat Until \"quit\" ---"
  loop
  where
    loop = do
      putStr "Enter text (type 'quit' to exit): "
      hFlush stdout
      input <- getLine
      if input == "quit"
        then putStrLn "Goodbye!"
        else do
          putStrLn ("You entered: " ++ input)
          loop

-- HC11T6: Uppercase Converter
hc11t6 :: IO ()
hc11t6 = do
  putStrLn "--- HC11T6: Uppercase Converter ---"
  putStr "Enter a line: "
  hFlush stdout
  line <- getLine
  putStrLn ("Uppercase: " ++ map toUpper line)

-- HC11T7: User Options
hc11t7 :: IO ()
hc11t7 = do
  putStrLn "--- HC11T7: User Options ---"
  putStrLn "Choose an option:"
  putStrLn "1. Greet"
  putStrLn "2. Add two numbers"
  putStrLn "3. Exit"
  putStr "Enter your choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> putStrLn "Hello, user!"
    "2" -> do
      putStr "Enter first number: "
      hFlush stdout
      a <- readLn
      putStr "Enter second number: "
      hFlush stdout
      b <- readLn
      putStrLn ("Sum: " ++ show (a + b))
    "3" -> putStrLn "Exiting..."
    _   -> putStrLn "Invalid option."

-- HC11T8: Even or Odd Checker
hc11t8 :: IO ()
hc11t8 = do
  putStrLn "--- HC11T8: Even or Odd Checker ---"
  putStr "Enter a number: "
  hFlush stdout
  input <- getLine
  let n = read input :: Int
  if even n
    then putStrLn "Even"
    else putStrLn "Odd"

-- HC11T9: Sum Two Numbers
hc11t9 :: IO ()
hc11t9 = do
  putStrLn "--- HC11T9: Sum Two Numbers ---"
  putStr "Enter first number: "
  hFlush stdout
  a <- readLn
  putStr "Enter second number: "
  hFlush stdout
  b <- readLn
  putStrLn ("Sum: " ++ show (a + b))

-- HC11T10: Reverse User Input
hc11t10 :: IO ()
hc11t10 = do
  putStrLn "--- HC11T10: Reverse User Input ---"
  putStr "Enter text: "
  hFlush stdout
  input <- getLine
  putStrLn ("Reversed: " ++ reverse input)

-- Menu to choose a task
main :: IO ()
main = do
  putStrLn "Choose a task (1-10):"
  hFlush stdout
  choice <- getLine
  case choice of
    "1"  -> hc11t1
    "2"  -> hc11t2
    "3"  -> hc11t3
    "4"  -> hc11t4
    "5"  -> hc11t5
    "6"  -> hc11t6
    "7"  -> hc11t7
    "8"  -> hc11t8
    "9"  -> hc11t9
    "10" -> hc11t10
    _    -> putStrLn "Invalid choice!"
