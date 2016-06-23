{-# OPTIONS_GHC -Wall #-}
module HW04 where

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

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P lst) = showHelper (reverse lst) ((length lst) - 1) True

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

