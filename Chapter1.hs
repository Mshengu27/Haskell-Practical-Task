import Data.List (sortBy)
import Data.Ord (comparing)

-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r ^ 2


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18


-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- Main function to test all tasks
main :: IO ()
main = do
    putStrLn "HC1T1 - doubleThenIncrement of 5:"
    print (doubleThenIncrement 5)

    putStrLn "\nHC1T2 - circleArea with radius 3:"
    print (circleArea 3)

    putStrLn "\nHC1T3 - greaterThan18 with 20 and 15:"
    print (greaterThan18 20)
    print (greaterThan18 15)

    putStrLn "\nHC1T4 - getTopThreePlayers:"
    let players = [("Alice", 50), ("Bob", 80), ("Charlie", 70), ("Diana", 90)]
    print (getTopThreePlayers players)

    putStrLn "\nHC1T5 - firstN 10 from infiniteNumbers:"
    print (firstN 10)

    putStrLn "\nHC1T6 - addNumbers 7 and 8:"
    print (addNumbers 7 8)

    putStrLn "\nHC1T7 - fToC 98.6 (body temp in F):"
    print (fToC 98.6)

    putStrLn "\nHC1T8 - applyTwice (*2) 5:"
    print (applyTwice (*2) 5)
