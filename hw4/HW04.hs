
{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a] deriving (Eq)

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

--instance (Num a, Eq a) => Eq (Poly a) where
--    (==) = undefined
 
-- Exercise 3 -----------------------------------------

showHelper :: (Num a, Eq a, Show a) => [a] -> Int -> Bool -> [Char]
showHelper (x:xs) exp first
  | x == 0 = showHelper xs (exp-1) first
  | x /= 0 && exp > 0 =
    let prefix = if first then "" else " + " in
    let suffix = if exp > 1 then "^" ++ (show exp) else "" in
        prefix ++ (show x) ++ "x" ++ suffix ++ showHelper xs (exp-1) False 
  | exp == 0 = let prefix = if first then "" else " + " in
      prefix ++ (show x)
  | otherwise =  ""

showHelper [] _ _ = ""

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P lst) = showHelper (reverse lst) ((length lst) - 1) True

-- Exercise 4 -----------------------------------------

listPlus :: Num a => [a] -> [a] -> [a]
listPlus (x:xs) (x2:xs2) = (x+x2):(listPlus xs xs2)
listPlus (x:xs) [] = x:xs
listPlus [] (x:xs) = x:xs
listPlus [] [] = []

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l1) (P l2) = P (listPlus l1 l2)

-- Exercise 5 -----------------------------------------

timesHelper :: Num a => [a] -> Int -> Poly a -> [Poly a]
timesHelper (x:xs) exp (P lst) =
  (P ((replicate exp 0) ++ (map ((*) x) lst))) : (timesHelper xs (exp +1) (P lst))
timesHelper [] _ _ = []

times :: Num a => Poly a -> Poly a -> Poly a
times (P l1) (P l2) = foldl' (+) (P [0]) (timesHelper l1 0 (P l2))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P lst) = P (map negate lst) 
    fromInteger x = P [fromInteger x]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyHelper :: Num a => [a] -> Int -> a -> a
applyHelper (x:xs) exp val = x * val^exp + applyHelper xs (exp+1) val
applyHelper [] _ _ = fromInteger 0

applyP :: Num a => Poly a -> a -> a
applyP (P (x:xs)) val = applyHelper (x:xs) 0 val

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n val 
       | n == 0 = val
       | n /= 0 = nderiv (n-1) (deriv val)

-- Exercise 9 -----------------------------------------

instance (Enum a, Num a) => Differentiable (Poly a) where
    deriv (P [_]) = P [0]
    deriv (P (x:xs)) = P (map (\x -> (fst x) * (snd x)) (zip [1..] xs))

