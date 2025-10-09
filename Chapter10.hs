-- Haskell Chapter 10 Practical Tasks: Custom Type Classes and Instances

-- HC10T1: ShowSimple Type Class
data PaymentMethod = Cash | Card | Crypto deriving (Show)

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash   = "Cash"
  showSimple Card   = "Card"
  showSimple Crypto = "Cryptocurrency"

-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

-- HC10T3: Comparable Type Class
data Blockchain = Bitcoin | Ethereum | Cardano deriving (Show, Ord)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith = compare

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance (Eq a) => Eq (Box a) where
  Empty == Empty = True
  Has x == Has y = x == y
  _ == _         = False

-- HC10T5: ShowDetailed Type Class
data User = User { userName :: String, userAge :: Int }

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User n a) = "User: " ++ n ++ ", Age: " ++ show a

-- HC10T6: Mutual Recursion in Eq for Blockchain (Fixed)
instance Eq Blockchain where
  Bitcoin  == Bitcoin  = True
  Ethereum == Ethereum = True
  Cardano  == Cardano  = True
  _ == _               = False
  a /= b               = not (a == b)  -- mutual recursion one-way

-- HC10T7: Convertible Type Class
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash   = "Cash Payment"
  convert Card   = "Card Payment"
  convert Crypto = "Cryptocurrency Payment"

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality a b = a == b

instance AdvancedEq Int
instance AdvancedEq Blockchain

-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable String where
  concatWith = (++)

-- MAIN FUNCTION TO TEST ALL TASKS
main :: IO ()
main = do
  putStrLn "--- HC10T1: ShowSimple ---"
  print (showSimple Cash)
  print (showSimple Card)
  print (showSimple Crypto)

  putStrLn "\n--- HC10T2: Summable ---"
  print (sumUp ([1, 2, 3, 4, 5] :: [Int]))

  putStrLn "\n--- HC10T3: Comparable ---"
  print (compareWith Bitcoin Cardano)

  putStrLn "\n--- HC10T4: Eq for Box ---"
  print (Has 10 == Has 10)
  print (Empty == Has 5)

  putStrLn "\n--- HC10T5: ShowDetailed ---"
  let user1 = User "Alice" 30
  print (showDetailed user1)

  putStrLn "\n--- HC10T6: Mutual Recursion Eq ---"
  print (Bitcoin == Bitcoin)
  print (Bitcoin /= Cardano)

  putStrLn "\n--- HC10T7: Convertible ---"
  print (convert Cash   :: String)
  print (convert Card   :: String)
  print (convert Crypto :: String)

  putStrLn "\n--- HC10T8: AdvancedEq ---"
  print (compareEquality (5 :: Int) 5)
  print (compareEquality Bitcoin Ethereum)

  putStrLn "\n--- HC10T9: MinMax ---"
  print (minValue :: Int)
  print (maxValue :: Int)

  putStrLn "\n--- HC10T10: Concatenatable ---"
  print (concatWith "Hello, " "World!")
  print (concatWith "Hello, " "Programmer!")
