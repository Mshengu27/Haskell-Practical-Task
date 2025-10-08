-- Haskell Chapter 9 Practical Tasks: Parametric Types and Recursion

-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a
  deriving (Show)

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (x + n)
addN _ Empty = Empty

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def (Has x) = x
extract def Empty = def

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Double }
  | Rectangle { color :: a, width :: Double, height :: Double }
  deriving (Show)

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content :: String,
    likes :: Int,
    comments :: [Tweet]
  }
  deriving (Show)

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Data Type
data Sequence a
  = End
  | Node a (Sequence a)
  deriving (Show)

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node v rest)
  | x == v = True
  | otherwise = elemSeq x rest

-- HC9T10: Binary Search Tree Data Type (using unique constructors)
data BST a
  = EmptyBST
  | NodeBST a (BST a) (BST a)
  deriving (Show)

insertBST :: Ord a => a -> BST a -> BST a
insertBST x EmptyBST = NodeBST x EmptyBST EmptyBST
insertBST x (NodeBST val left right)
  | x == val = NodeBST val left right
  | x < val = NodeBST val (insertBST x left) right
  | otherwise = NodeBST val left (insertBST x right)

searchBST :: Ord a => a -> BST a -> Bool
searchBST _ EmptyBST = False
searchBST x (NodeBST val left right)
  | x == val = True
  | x < val = searchBST x left
  | otherwise = searchBST x right

inOrder :: BST a -> [a]
inOrder EmptyBST = []
inOrder (NodeBST v l r) = inOrder l ++ [v] ++ inOrder r

-- MAIN FUNCTION THAT TEST ALL TASKS
main :: IO ()
main = do
  putStrLn "--- HC9T1: Parametric Type Synonym ---"
  let addressEntity :: Entity String
      addressEntity = ("Office", "123 Main Street")
  print addressEntity

  putStrLn "\n--- HC9T2: Parametric Data Type (Box) ---"
  let box1 = Has 10
      box2 :: Box Int
      box2 = Empty
  print box1
  print box2

  putStrLn "\n--- HC9T3: Add Values in a Box ---"
  print (addN 5 box1)
  print (addN 5 box2)

  putStrLn "\n--- HC9T4: Extract from a Box ---"
  print (extract 0 box1)
  print (extract 0 box2)

  putStrLn "\n--- HC9T5: Parametric Shape Type ---"
  let circle = Circle "Red" 5.0
      rect = Rectangle "Blue" 4.0 6.0
  print circle
  print rect

  putStrLn "\n--- HC9T6 & HC9T7: Tweets and Engagement ---"
  let reply1 = Tweet "Nice post!" 3 []
      reply2 = Tweet "Awesome!" 5 []
      mainTweet = Tweet "Learning Haskell!" 10 [reply1, reply2]
  print mainTweet
  putStrLn $ "Total Engagement: " ++ show (engagement mainTweet)

  putStrLn "\n--- HC9T8 & HC9T9: Sequence ---"
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1
  print (elemSeq 2 seq1)
  print (elemSeq 5 seq1)

  putStrLn "\n--- HC9T10: Binary Search Tree (BST) ---"
  let emptyTree = EmptyBST
      tree = foldr insertBST emptyTree [10, 5, 15, 2, 7, 12, 20]
  print tree
  putStrLn $ "Is 7 in the tree? " ++ show (searchBST 7 tree)
  putStrLn $ "Is 99 in the tree? " ++ show (searchBST 99 tree)
  putStrLn $ "In-order traversal: " ++ show (inOrder tree)
