-- HC3T1 - Task 1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n =
    if n > 0 then "Positive"
    else if n < 0 then "Negative"
    else "Zero"

-- Tests:
-- checkNumber 5      => "Positive"
-- checkNumber (-3)   => "Negative"
-- checkNumber 0      => "Zero"


-- HC3T2 - Task 2: Determine the grade based on a score using guards
grade :: Int -> String
grade n
    | n >= 90   = "A"
    | n >= 80   = "B"
    | n >= 70   = "C"
    | n >= 60   = "D"
    | otherwise = "F"

-- Tests:
-- grade 95 => "A"
-- grade 72 => "C"
-- grade 50 => "F"


-- HC3T3 - Task 3: Convert RGB to hex string using let bindings
import Text.Printf (printf)

rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let hexR = printf "%02X" r
        hexG = printf "%02X" g
        hexB = printf "%02X" b
    in hexR ++ hexG ++ hexB

-- Tests:
-- rgbToHex (255, 0, 127) => "FF007F"
-- rgbToHex (0, 255, 64)  => "00FF40"


-- HC3T4 - Task 4: Triangle area (Heron's formula)
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- Tests:
-- triangleArea 3 4 5 => 6.0
-- triangleArea 7 8 9 => 26.83...


-- HC3T5 - Task 5: Triangle type using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

-- Tests:
-- triangleType 3 3 3 => "Equilateral"
-- triangleType 5 5 8 => "Isosceles"
-- triangleType 6 7 8 => "Scalene"


-- HC3T6 - Advanced Task 6: Leap year check
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

-- Tests:
-- isLeapYear 2000 => True
-- isLeapYear 1900 => False
-- isLeapYear 2024 => True


-- HC3T7 - Advanced Task 7: Season by month
season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Winter"
    | m >= 3 && m <= 5            = "Spring"
    | m >= 6 && m <= 8            = "Summer"
    | m >= 9 && m <= 11           = "Autumn"
    | otherwise                   = "Invalid month"

-- Tests:
-- season 3  => "Spring"
-- season 7  => "Summer"
-- season 11 => "Autumn"


-- HC3T8 - Advanced Task 8: BMI category
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25.0 = "Normal"
    | bmi < 30.0 = "Overweight"
    | otherwise  = "Obese"
    where bmi = weight / (height ^ 2)

-- Tests:
-- bmiCategory 70 1.75 => "Normal"
-- bmiCategory 90 1.8  => "Overweight"


-- HC3T9 - Advanced Task 9: Max of three numbers
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = max a b
        maxAll = max maxAB c
    in maxAll

-- Tests:
-- maxOfThree 10 20 15 => 20
-- maxOfThree 5 25 10  => 25


-- HC3T10 - Advanced Task 10: Palindrome check
isPalindrome :: String -> Bool
isPalindrome s
    | length s <= 1 = True
    | head s == last s = isPalindrome (init (tail s))
    | otherwise = False

-- Tests:
-- isPalindrome "racecar" => True
-- isPalindrome "haskell" => False
-- isPalindrome "madam"   => True
