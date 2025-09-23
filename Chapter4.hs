-- HC4T1 - Task 1: Define a weatherReport Function
-- This function takes a weather condition as input and uses pattern matching
-- to return a descriptive message.
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"  -- catch-all for other cases


-- HC4T2 - Task 2: Define a dayType Function
-- This function determines if a given day is a weekday or weekend.
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"  -- catch-all for invalid input


-- HC4T3 - Task 3: Define a gradeComment Function
-- This function takes a numerical grade and returns feedback.
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89  = "Good job!"
  | n >= 50 && n <= 69  = "You passed."
  | n >= 0  && n <= 49  = "Better luck next time."
  | otherwise           = "Invalid grade"


-- HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
-- The original version probably used if-else. Now we’ll use pattern matching.
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! Very special!"
specialBirthday 18 = "18th birthday! Welcome to adulthood!"
specialBirthday 21 = "21st birthday! Cheers!"
specialBirthday _  = "Just another birthday."


-- HC4T5 - Task 5: Add a Catch-All Pattern with a Custom Message
-- Now we include the age in the return message when it’s not a special case.
specialBirthday' :: Int -> String
specialBirthday' 1  = "First birthday! Very special!"
specialBirthday' 18 = "18th birthday! Welcome to adulthood!"
specialBirthday' 21 = "21st birthday! Cheers!"
specialBirthday' age = "Just a regular birthday at age " ++ show age ++ "."


-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
-- This function checks the length of the list using pattern matching.
whatsInsideThisList :: [a] -> String
whatsInsideThisList []      = "The list is empty."
whatsInsideThisList [x]     = "The list has one element."
whatsInsideThisList [x,y]   = "The list has two elements."
whatsInsideThisList (x:xs)  = "The list has many elements."


-- HC4T7 - Task 7: Ignore Elements in a List
-- Return only the first and third elements of a list, ignoring others.
firstAndThird :: [a] -> (a, a)
firstAndThird (x:_:z:_) = (x, z)  -- take 1st and 3rd
firstAndThird _         = error "List does not have enough elements."


-- HC4T8 - Task 8: Extract Values from Tuples
-- This function extracts and describes values from a tuple.
describeTuple :: (Int, String, Bool) -> String
describeTuple (n, str, flag) =
  "The number is " ++ show n ++
  ", the string is \"" ++ str ++ "\"" ++
  ", and the boolean is " ++ show flag ++ "."


-- Main function to test all tasks
main :: IO ()
main = do
  putStrLn "--- HC4T1: weatherReport ---"
  print (weatherReport "sunny")
  print (weatherReport "rainy")
  print (weatherReport "windy")

  putStrLn "\n--- HC4T2: dayType ---"
  print (dayType "Monday")
  print (dayType "Saturday")
  print (dayType "Funday")

  putStrLn "\n--- HC4T3: gradeComment ---"
  print (gradeComment 95)
  print (gradeComment 75)
  print (gradeComment 55)
  print (gradeComment 20)
  print (gradeComment 120)

  putStrLn "\n--- HC4T4 & HC4T5: specialBirthday ---"
  print (specialBirthday 1)
  print (specialBirthday 18)
  print (specialBirthday 21)
  print (specialBirthday 10)
  print (specialBirthday' 10)

  putStrLn "\n--- HC4T6: whatsInsideThisList ---"
  print (whatsInsideThisList ([] :: [Int]))
  print (whatsInsideThisList [42])
  print (whatsInsideThisList [1,2])
  print (whatsInsideThisList [1,2,3,4])

  putStrLn "\n--- HC4T7: firstAndThird ---"
  print (firstAndThird [10,20,30,40])

  putStrLn "\n--- HC4T8: describeTuple ---"
  print (describeTuple (7, "hello", True))
