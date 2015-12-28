module Questions21to28
( insertAt
, insertListAt
, range
, combinations
) where

import Data.List

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n =
     first ++ c:rest
     where (first, rest) = splitAt (n-1) xs

insertListAt :: [a] -> [a] -> Int -> [a]
insertListAt list xs n =
     first ++ list ++ rest
     where (first, rest) = splitAt (n-1) xs

-- Problem 22
range :: Int -> Int -> [Int]
range start end
    | start < end = start : range (succ start) end
    | start > end = start : range (pred start) end
    | otherwise = [start]

-- if start > end was not supported then the following 2 would suffice
{-range start end = [start .. end]-}
{-range = enumFromTo-}

-- Problem 23
-- Skipped (random)

-- Problem 24
-- Skipped (random)

-- Problem 25
-- Skipped (random)

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations k xs = filter ((k==) . length) . Data.List.subsequences $ xs

-- Problem 27
-- Skipped

-- Problem 28
-- Skipped
