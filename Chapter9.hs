-- HC9T1: Define a Parametric Type Synonym
-- Represents an entity with a name and an address or identifier of any type.
type Entity a = (String, a)

-- HC9T2: Define a Parametric Data Type
-- A Box can either be empty or contain a value of any type.
data Box a = Empty | Has a deriving Show

-- HC9T3: Function to Add Values in a Box
-- Adds a number to the value inside the box if it exists.
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (n + x)
addN _ Empty   = Empty

-- HC9T4: Function to Extract a Value from a Box
-- Returns the value inside the box or a default value if the box is empty.
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

-- HC9T5: Parametric Data Type with Record Syntax
-- Represents shapes with a color field.
data Shape a = Circle { color :: a }
             | Rectangle { color :: a } deriving Show

-- HC9T6: Recursive Data Type for Tweets
-- A Tweet contains content, likes, and a list of comments (which are also Tweets).
data Tweet = Tweet {
    content  :: String,
    likes    :: Int,
    comments :: [Tweet]
} deriving Show

-- HC9T7: Engagement Function for Tweets
-- Calculates total engagement by summing likes and engagement of comments.
engagement :: Tweet -> Int
engagement (Tweet _ likes comments) = likes + sum (map engagement comments)

-- HC9T8: Recursive Sequence Data Type
-- Represents a linear sequence of values.
data Sequence a = End | Node a (Sequence a) deriving Show

-- HC9T9: Function to Check for Element in a Sequence
-- Returns True if the element is found in the sequence.
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest) = x == y || elemSeq x rest

-- HC9T10: Binary Search Tree Data Type
-- Represents a binary search tree with values and left/right subtrees.
data BST a = EmptyTree
           | NodeBST a (BST a) (BST a) deriving Show

-- Main function to demonstrate all tasks
main :: IO ()
main = do
    -- HC9T1: Entity example
    let homeAddress = ("Home", "123 Main Street") :: Entity String
    putStrLn $ "Entity: " ++ show homeAddress

    -- HC9T2–HC9T4: Box operations
    let box = Has 10
    putStrLn $ "Original box: " ++ show box
    putStrLn $ "Box after adding 5: " ++ show (addN 5 box)
    putStrLn $ "Extracted value: " ++ show (extract 0 box)
    putStrLn $ "Extract from empty box: " ++ show (extract 0 Empty)

    -- HC9T5: Shape examples
    let redCircle = Circle { color = "Red" }
    let blueRect = Rectangle { color = "Blue" }
    putStrLn $ "Circle: " ++ show redCircle
    putStrLn $ "Rectangle: " ++ show blueRect

    -- HC9T6–HC9T7: Tweet and engagement
    let comment1 = Tweet "Nice!" 2 []
    let comment2 = Tweet "Great post!" 3 []
    let mainTweet = Tweet "Hello World" 10 [comment1, comment2]
    putStrLn $ "Main Tweet: " ++ show mainTweet
    putStrLn $ "Total Engagement: " ++ show (engagement mainTweet)

    -- HC9T8–HC9T9: Sequence and element check
    let seq1 = Node 1 (Node 2 (Node 3 End))
    putStrLn $ "Sequence: " ++ show seq1
    putStrLn $ "Contains 2? " ++ show (elemSeq 2 seq1)
    putStrLn $ "Contains 5? " ++ show (elemSeq 5 seq1)

    -- HC9T10: Binary Search Tree example
    let tree = NodeBST 5 (NodeBST 3 EmptyTree EmptyTree) (NodeBST 7 EmptyTree EmptyTree)
    putStrLn $ "Binary Search Tree: " ++ show tree
