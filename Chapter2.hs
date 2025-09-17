import Data.List (sortBy)
import Data.Ord (comparing)

--HC2T1 - Checking Types in GHCi
exampleTuple :: (Int ,Double ,String , Char , Bool)
exampleTuple = (42, 3.14 ,"Haskell", 'Z', True && False)

main :: IO()
main = print exampleTuple

-- HC2T2 - Function Type Signatures
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2


-- HC2T3 - Immutable Variables
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True


-- HC2T5 - Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r ^ 2

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)


-- HC2T6 - Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127


-- HC2T7 - Boolean Expressions
expr1 :: Bool
expr1 = True && (5 > 3)     -- True

expr2 :: Bool
expr2 = False || (2 > 5)    -- False

expr3 :: Bool
expr3 = not False           -- True

expr4 :: Bool
expr4 = 10 < 3              -- False


-- Main to demonstrate all HC2 tasks
main :: IO ()
main = do
    putStrLn "===== HC2T1 - Checking Types in GHCi (expected) ====="
    putStrLn "42            :: Int"
    putStrLn "3.14          :: Fractional a => a (default Double)"
    putStrLn "\"Haskell\"     :: String (aka [Char])"
    putStrLn "'Z'           :: Char"
    putStrLn "True && False :: Bool"
    putStrLn ""

    putStrLn "===== HC2T2 - Function Type Signatures ====="
    putStrLn $ "add 5 7 = " ++ show (add 5 7)
    putStrLn $ "isEven 10 = " ++ show (isEven 10)
    putStrLn $ "isEven 7 = " ++ show (isEven 7)
    putStrLn $ "concatStrings \"Hi \" \"there\" = " ++ show (concatStrings "Hi " "there")
    putStrLn ""

    putStrLn "===== HC2T3 - Immutable Variables ====="
    putStrLn $ "myAge = " ++ show myAge
    putStrLn $ "piValue = " ++ show piValue
    putStrLn $ "greeting = " ++ greeting
    putStrLn $ "isHaskellFun = " ++ show isHaskellFun
    putStrLn "(Trying to reassign myAge would cause a compile error!)"
    putStrLn ""

    putStrLn "===== HC2T4 - Infix vs Prefix Notation ====="
    putStrLn $ "Infix 5 + 3   = " ++ show (5 + 3)
    putStrLn $ "Prefix (+) 5 3 = " ++ show ((+) 5 3)
    putStrLn $ "Infix 10 * 4   = " ++ show (10 * 4)
    putStrLn $ "Prefix (*) 10 4 = " ++ show ((*) 10 4)
    putStrLn $ "Infix True && False   = " ++ show (True && False)
    putStrLn $ "Prefix (&&) True False = " ++ show ((&&) True False)
    putStrLn ""

    putStrLn "===== HC2T5 - Defining and Using Functions ====="
    putStrLn $ "circleArea 3 = " ++ show (circleArea 3)
    putStrLn $ "circleArea 5 = " ++ show (circleArea 5)
    putStrLn $ "maxOfThree 3 7 5 = " ++ show (maxOfThree 3 7 5)
    putStrLn $ "maxOfThree 10 2 8 = " ++ show (maxOfThree 10 2 8)
    putStrLn ""

    putStrLn "===== HC2T6 - Int vs Integer ====="
    putStrLn $ "smallNumber (Int) = " ++ show smallNumber
    putStrLn $ "bigNumber (Integer) = " ++ show bigNumber
    putStrLn "Try in GHCi: 2^64 :: Int   --> usually overflows (wraps to 0)"
    putStrLn "Try in GHCi: 2^64 :: Integer --> gives 18446744073709551616"
    putStrLn ""

    putStrLn "===== HC2T7 - Boolean Expressions ====="
    putStrLn $ "True using &&: " ++ show expr1
    putStrLn $ "False using ||: " ++ show expr2
    putStrLn $ "True using not: " ++ show expr3
    putStrLn $ "Comparison returning False: " ++ show expr4
