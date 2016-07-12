{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 (tail fibs2))
-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : (streamToList y)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x y) = Cons (f x) (fmap f y)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f seed = Cons seed (sIterate f (f seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x y) z = Cons x (sInterleave z y)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x y) = x : (sTake (n-1) y)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (1+) 0


ruler :: Stream Integer
ruler = foldl1 sInterleave (map sRepeat [0..])

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate (\x -> mod (1103515245 * x + 12345) 2147483648) seed

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxHelper :: Int -> Int -> [Int] -> Maybe (Int, Int)
minMaxHelper x y [] = Just (x, y)
minMaxHelper min max (x:xs)
  | x < min = minMaxHelper x max xs
  | x > max = minMaxHelper min x xs
  | otherwise = minMaxHelper min max xs

minMax :: [Int] -> Maybe (Int, Int)
minMax []  = Nothing
minMax (x:xs) = minMaxHelper x x xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Mat2d Int Int Int Int

mat2dPlus = undefined
mat2dTimes = undefined

instance Num Matrix where
  (+) = mat2dPlus
  (*) = mat2dTimes

fastFib :: Int -> Integer
fastFib = undefined
