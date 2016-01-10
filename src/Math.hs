module Math where

-- https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
