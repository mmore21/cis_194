{-# OPTIONS_GHC -Wall #-}

module Main where

-- Convert an Integer to a list of its digits
toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
  | otherwise = []

-- Convert an Integer to a list of its digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

-- Doubles every other Integer in a list, beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther list@(x:xs)
  | (length list) `mod` 2 == 0 = (x * 2) : doubleEveryOther xs
  | otherwise = x : doubleEveryOther xs

-- Sum all digits from an Integer list
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

-- Validate whether an Integer is an actual credit card
validate :: Integer -> Bool
validate number = total `mod` 10 == 0
  where total = (sumDigits . doubleEveryOther . toDigits) number

-- Calculate Towers of Hanoi moves
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source target _ = [(source, target)]
hanoi n source target tmp =
  (hanoi (n - 1) source tmp target) ++
  [(source, target)] ++
  (hanoi (n - 1) tmp target source)

main :: IO ()
main = do
  print (toDigits 1234)
  print (toDigitsRev 1234)
  print (doubleEveryOther [1,2,3,4])
  print (sumDigits [1,2,3,11,22,33])
  print (validate 4012888888881881)
  print (hanoi 2 "a" "b" "c")
