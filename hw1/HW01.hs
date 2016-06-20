{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = mod x 10 

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = [0]
toRevDigits x = if x < 10
  then [x]
  else (lastDigit x) : toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = (x:[])
doubleEveryOther (x:y:xs) = x:(y*2):(doubleEveryOther xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9     = (x `div` 10) + (mod x 10) + (sumDigits xs)
  | otherwise = x + (sumDigits xs) 


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = ((==) 0) . sumDigits . doubleEveryOther . toRevDigits 

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
