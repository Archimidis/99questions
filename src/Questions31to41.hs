module Questions31to41
( isPrime
, isPrimeV2
, myGCD
, coprime
, totient
, primeFactors
, primeFactorsMult
, totientImproved
, primesR
, goldbach
, goldbachList
, goldbachList'
) where

import Data.List
import Control.Arrow

-- Problem 31
isPrime :: Int -> Bool
isPrime n | n < 4 = n > 1
isPrime n =
    all ((/=0) . mod n) [2 .. top]
    where top = floor . sqrt $ (fromIntegral n :: Float)

isPrimeV2 :: Int -> Bool
isPrimeV2 n | n < 4 = n > 1
isPrimeV2 n =
    all ((/=0) . mod n) $ takeWhile (<= top) candidates
    where candidates = 2 : 3 : [x + i | x <- [6, 12 ..], i <- [-1, 1]]
          top = floor . sqrt $ (fromIntegral n :: Float)

-- Problem 32
myGCD :: Int -> Int -> Int
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = 1 == gcd a b

-- Problem 34
-- Euler's totient function phi(m)
totient :: Int -> Int
totient 1 = 1
totient m = length . filter id $ map (coprime m) [1 .. (pred m)]

-- Problem 35
nextPrime :: Int -> Int
nextPrime n
    | isPrimeV2 candidate = candidate
    | otherwise = nextPrime candidate
    where candidate = succ n

-- Don't care about performance right now
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n | isPrimeV2 n = [n]
primeFactors n =
    nextPrimeDivisor 2 n : primeFactors next
    where nextPrimeDivisor prime number
            | number `mod` prime == 0 = prime
            | otherwise = nextPrimeDivisor (nextPrime prime) number
          next = n `div` nextPrimeDivisor 2 n

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult =
    map (head &&& length) . group . sort . primeFactors

-- Problem 37
totientImproved :: Int -> Int
totientImproved =
    product . map (\(p, m) -> (p - 1) * p^(m - 1)) . primeFactorsMult

-- Problem 38
{-
 - Comparing the 2 implementations of Euler's totient function on my machine we
 - have the following results in execution time and memory consumption:
 -
 - totient 100000 == 40000
 - (0.09 secs, 98,485,352 bytes)
 -
 - VS
 -
 - totientImproved 100000 == 40000
 - (0.01 secs, 1,585,160 bytes) !!!!
 -}

-- Problem 39
primesR :: Int -> Int -> [Int]
primesR low high
    | low > high = []
    | isPrimeV2 low = low : primesR (nextPrime low) high
    | otherwise = primesR (nextPrime low) high

-- Problem 40
-- Goldbach's conjecture
goldbach :: Int -> (Int, Int)
goldbach n
    | odd n || n < 2 = error "Goldbach conjecture works positive even number greater than 2"
goldbach n =
    head [(x,y) | x <- primes, y <- primes, x + y == n]
    where primes = primesR 2 (n - 2)

-- Problem 41
goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList low high = [goldbach x | x <- [low .. high],  even x]

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' low high threshold  = 
    filter (\(x,y) -> x >= threshold && y >= threshold) $ goldbachList low high
