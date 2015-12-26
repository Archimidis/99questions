module Questions11to20
( EncodedValue(Single, Multiple)
, encodeModified
, decodeModified
, encodeDirect
, dupli
, repli
, dropEvery
, split
, splitv2
, splitv3
, splitv4
, slice
, rotate
, removeAt
, removeAtv2
) where

import qualified Data.List
import qualified Control.Arrow

-- Problem 11
data EncodedValue a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: (Ord a) => [a] -> [EncodedValue a]
encodeModified =
    map (format . (length Control.Arrow.&&& head)) . Data.List.group
    where format (1, x) = Single x
          format (n, x) = Multiple n x

-- Problem 12
decodeModified :: [EncodedValue a] -> [a]
decodeModified =
    concatMap decode
    where decode (Single x) = [x]
          decode (Multiple n x) = replicate n x

-- Problem 13
encodeDirect :: (Ord a) => [a] -> [EncodedValue a]
encodeDirect [] = []
encodeDirect (x:xs)
    | n == 1 = Single x : encodeDirect rest
    | otherwise = Multiple n x : encodeDirect rest
    where (same, rest) = span (==x) xs
          n = 1 + length same

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
    map fst . filter (\(_, place) -> place `mod` n /= 0) $ zip xs [1..]

-- initial solution
{-dropEvery [] _ = []-}
{-dropEvery xs n-}
    {-| null rest = keep-}
    {-| otherwise = keep ++ dropEvery (tail rest) n-}
    {-where (keep, rest) = Data.List.splitAt (n-1) xs-}

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split (x:xs) n
    | n > 0 = (x:first, rest)
    where (first, rest) = split xs (n-1)
split xs _ = ([], xs)

splitv2 :: [a] -> Int -> ([a], [a])
splitv2 xs n =
    splitHelper xs n [] []

splitHelper :: [a] -> Int -> [a] -> [a] -> ([a], [a])
splitHelper [] _ left right = (left, right)
splitHelper (x:rest) i left right
    | i > 0 = splitHelper rest (i-1) (left ++ [x]) right
    | otherwise = splitHelper rest (i-1) left (right ++ [x])

splitv3 :: [a] -> Int -> ([a], [a])
splitv3 xs n = (low, high)
    where counters = zip [1..] xs
          low = map snd $ filter (\(i, _) -> i <= n) counters
          high = map snd $ filter (\(i, _) -> i > n) counters

-- take, drop and splitAt are considered predefined predicates
splitv4 :: [a] -> Int -> ([a], [a])
splitv4 xs n = (take n xs, drop n xs)

-- Problem 18
type StartIndex = Int
type EndIndex = Int

slice :: [a] -> StartIndex -> EndIndex -> [a]
slice xs i k
    | i > 0 = take (k-i+1) . drop (i-1) $ xs
    | otherwise = error "Negative bound given"

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate (x:xs) n
    | n > 0 = rotate (xs ++ [x]) (n-1)
    | n < 0 = rotate (last xs : x : init xs) (n+1)
    | otherwise = x:xs

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = 
    (removed, start ++ end)
    where removed = xs !! (k-1)
          start = take (k-1) xs
          end = drop k xs

removeAtv2 :: Int -> [a] -> (a, [a])
removeAtv2 n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)
