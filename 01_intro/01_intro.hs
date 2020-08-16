module Main where

toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : y : zs) = x * 2 : y : doubleEveryOther zs


main :: IO ()
main = do
  print (toDigits 1234)
  print (toDigitsRev 1234)
  print (doubleEveryOther [1,2,3,4])
