-- Haskell Chapter 8 Practical Tasks: Data Types, Synonyms, and Records
-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value   = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val =
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person {
    personName    :: String,
    personAddress :: (String, Int),
    paymentMethod :: PaymentMethod
} deriving Show

bob :: Person
bob = Person { personName = "Bob", personAddress = ("Main Street", 101), paymentMethod = Cash }

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h

-- HC8T4: Record Syntax for Employee
data Employee = Employee { name :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

-- HC8T5: Record Syntax for Person
data PersonRec = PersonRec { pname :: String, age :: Int, isEmployed :: Bool } deriving Show

person1 :: PersonRec
person1 = PersonRec { pname = "Alice", age = 30, isEmployed = True }

person2 :: PersonRec
person2 = PersonRec { pname = "Charlie", age = 25, isEmployed = False }

-- HC8T6: Record Syntax for Shape Variants
data ShapeRec
  = CircleRec { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleRec { width :: Float, height :: Float, color :: String }
  deriving Show

circleShape :: ShapeRec
circleShape = CircleRec { center = (0,0), color = "Red", radius = 5 }

rectangleShape :: ShapeRec
rectangleShape = RectangleRec { width = 10, height = 5, color = "Blue" }

-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 :: Animal
dog1 = Dog "Buddy"

cat1 :: Animal
cat1 = Cat "Whiskers"

-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age  = Int

greet :: Name -> Age -> String
greet n a = "Hello, my name is " ++ n ++ " and I am " ++ show a ++ " years old."

-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction {
    from :: Address,
    to :: Address,
    amount :: Value,
    transactionId :: String
} deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr val =
  let tx = Transaction { from = fromAddr, to = toAddr, amount = val, transactionId = "TX12345" }
  in transactionId tx

-- HC8T10: Deriving Show for Book
data Book = Book { title :: String, author :: String, year :: Int } deriving Show

myBook :: Book
myBook = Book { title = "Learn Haskell", author = "Tankiso Tshabalala", year = 2025 }

-- Main to run all tasks
main :: IO ()
main = do
  putStrLn "--- HC8T1: generateTx ---"
  putStrLn (generateTx "Alice" "Bob" 100)

  putStrLn "\n--- HC8T2: Person bob ---"
  print bob

  putStrLn "\n--- HC8T3: area ---"
  print (area (Circle 5))
  print (area (Rectangle 10 5))

  putStrLn "\n--- HC8T4: Employee richard ---"
  print richard

  putStrLn "\n--- HC8T5: Persons ---"
  print person1
  print person2

  putStrLn "\n--- HC8T6: ShapeRec ---"
  print circleShape
  print rectangleShape

  putStrLn "\n--- HC8T7: Animals ---"
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)

  putStrLn "\n--- HC8T8: greet ---"
  putStrLn (greet "David" 40)

  putStrLn "\n--- HC8T9: createTransaction ---"
  putStrLn (createTransaction "Alice" "Bob" 200)

  putStrLn "\n--- HC8T10: Book ---"
  print myBook
