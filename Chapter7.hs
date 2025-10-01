-- Chapter7.hs
-- Haskell Chapter 7 Practical Tasks: Type Classes and Custom Types

module Main where
import Text.Read (readMaybe)

-- HC7T1: Implement an Eq Instance for a Custom Data Type
data Color = Red | Green | Blue
  deriving (Show, Read, Enum, Bounded)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

-- HC7T2: Implement an Ord Instance for a Custom Data Type
instance Ord Color where
  compare Red Red     = EQ
  compare Green Green = EQ
  compare Blue Blue   = EQ
  compare Red _       = LT
  compare Green Red   = GT
  compare Green Blue  = LT
  compare Blue _      = GT

-- HC7T3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
  show (Circle r)        = "Circle with radius " ++ show r
  show (Rectangle w h)   = "Rectangle with width " ++ show w ++ " and height " ++ show h

instance Read Shape where
  readsPrec _ input =
    case words input of
      ["Circle", r] ->
        case readMaybe r of
          Just radius -> [(Circle radius, "")]
          Nothing     -> []
      ["Rectangle", w, h] ->
        case (readMaybe w, readMaybe h) of
          (Just width, Just height) -> [(Rectangle width height, "")]
          _                         -> []
      _ -> []

-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: Using Integral and Floating Type Classes
circleCircumference :: (Floating a, Integral b) => b -> a
circleCircumference r = 2 * pi * fromIntegral r

-- HC7T7: Bounded and Enum
nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise     = succ c

-- HC7T8: Parse a Value from a String Using Read
parseShape :: String -> Maybe Shape
parseShape str =
  case readMaybe str of
    Just s  -> Just s
    Nothing -> Nothing

-- HC7T9: Type Class with Multiple Instances
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is True."
  describe False = "This is False."

instance Describable Shape where
  describe (Circle r)      = "A circle of radius " ++ show r
  describe (Rectangle w h) = "A rectangle of width " ++ show w ++ " and height " ++ show h

-- Add Describable instance for Color
instance Describable Color where
  describe Red   = "The color Red"
  describe Green = "The color Green"
  describe Blue  = "The color Blue"

-- HC7T10: Function with Multiple Type Class Constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (compareValues x y)

-- ----------------
-- Main to test all tasks
-- ----------------
main :: IO ()
main = do
  putStrLn "--- HC7T1 & HC7T2: Eq & Ord for Color ---"
  print (Red == Red)       -- True
  print (Red < Green)      -- True
  print (Blue > Green)     -- True

  putStrLn "\n--- HC7T3: compareValues ---"
  print (compareValues 10 20)  -- 20
  print (compareValues 'a' 'z') -- 'z'

  putStrLn "\n--- HC7T4: Shape with Show & Read ---"
  print (show (Circle 5))
  print (show (Rectangle 3 4))
  print (readMaybe "Circle 10" :: Maybe Shape)      -- Just (Circle 10.0)
  print (readMaybe "Rectangle 6 8" :: Maybe Shape)  -- Just (Rectangle 6.0 8.0)

  putStrLn "\n--- HC7T5: squareArea ---"
  print (squareArea 5 :: Int)    -- 25
  print (squareArea 2.5 :: Double) -- 6.25

  putStrLn "\n--- HC7T6: circleCircumference ---"
  print (circleCircumference 5 :: Double) -- 31.4159...

  putStrLn "\n--- HC7T7: nextColor ---"
  print (nextColor Red)   -- Green
  print (nextColor Green) -- Blue
  print (nextColor Blue)  -- Red (wrap around)

  putStrLn "\n--- HC7T8: parseShape ---"
  print (parseShape "Circle 7")       -- Just (Circle 7.0)
  print (parseShape "Rectangle 3 9")  -- Just (Rectangle 3.0 9.0)
  print (parseShape "Invalid")        -- Nothing

  putStrLn "\n--- HC7T9: Describable ---"
  print (describe True)
  print (describe (Circle 4))
  print (describe (Rectangle 2 6))

  putStrLn "\n--- HC7T10: describeAndCompare ---"
  print (describeAndCompare Red Blue)   -- description of Blue
  print (describeAndCompare Green Red)  -- description of Green
