module Questions1to10
( NestedList(Elem, List)
  , myLast
  , myButLast
  , elementAt
  , myLength
  , myReverse
  , isPalindrome
  , flatten
  , compress
  , pack
  , encode
) where

import qualified Data.List
import qualified Control.Arrow

-- Problem 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast []  = error "empty list"
myButLast [x, _] = x
myButLast (_:xs)  = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt (x:xs) k
    | k  == 1 = x
    | otherwise = elementAt xs (k - 1)

-- Problem 4
myLength :: [a] -> Integer
myLength = foldr (\_ -> (+) 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem n) = [n]
flatten (List xs) = concatMap flatten xs

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress = map head . Data.List.group

-- Found out I can pattern match before @
{-compress (x:xs@(y:_))-}
    {-| x == y = compress xs-}
    {-| otherwise = x : compress xs-}
{-compress ys = ys-}

-- Initial solution
{-compress [] = []-}
{-compress xs = foldr (\x c -> if x == head c then c else x : c) [last xs] xs-}

-- Problem 9
pack :: (Ord a) => [a] -> [[a]]
pack [] = []
pack (x:xs) =
    (x : same) : pack rest
    where (same, rest) = Data.List.break (/=x) xs

-- Problem 10
encode :: (Ord a) => [a] -> [(Int, a)]
encode =
    map (length Control.Arrow.&&& head) . pack
