module Questions46to50 where

import Control.Arrow (second)
import Control.Monad (replicateM)
import Data.List

-- Problem 46
and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

-- Implies truth table
-- A B A=>B
-- T T T
-- T F F
-- F T T
-- F F T
impl' :: Bool -> Bool -> Bool
impl' a b = not a `or'` b

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table fn = [[a, b, fn a b] | a <- [True, False], b <- [True, False]]

-- Problem 47
infixl 7 `equ'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 5 `xor'`
infixl 4 `or'`
infixl 4 `nor'`

-- Problem 48
tablen :: Int -> ([Bool]-> Bool) -> [[Bool]]
tablen n fn = map (\x -> x ++ [fn x]) $ replicateM n [True, False]

-- Problem 49
gray :: Int -> [String]
gray 0 = [""]
gray n =
    map ('0':) previous ++ map ('1':) reflection
    where previous = gray (n-1)
          reflection = reverse previous

-- This is an efficient solution (copied)
grayV2 :: Integral a => a -> [String]
grayV2 0 = [""]
grayV2 n =
    Data.List.sort . foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ grayV2 (n-1)

-- Problem 50 (Huffman coding)
data HuffmanTree a = Leaf a | InternalNode (HuffmanTree a) (HuffmanTree a)
    deriving (Show, Read, Eq)

type Weight = Float
type Symbol = Char
type Code = String

huffman :: [(Symbol, Weight)] -> [(Symbol, Code)]
huffman [] = []
huffman xs =
    sortOn fst . generateCodes . createHuffmanTree . createLeaves $ xs
    where
        createLeaves list = [(weight, Leaf symbol) | (symbol, weight) <- list]

generateCodes :: HuffmanTree Char -> [(Symbol, Code)]
generateCodes (Leaf a) = [(a, "")]
generateCodes (InternalNode (Leaf a) (Leaf b)) =
    [(a, "0"), (b, "1")]
generateCodes (InternalNode (Leaf a) right) =
    (a, "0") : map (Control.Arrow.second ((:) '1')) (generateCodes right)
generateCodes (InternalNode left (Leaf a)) =
    map (Control.Arrow.second ((:) '0')) (generateCodes left) ++  [(a, "1")]
generateCodes (InternalNode left right) =
    map (Control.Arrow.second ((:) '0')) (generateCodes left)
    ++
    map (Control.Arrow.second ((:) '1')) (generateCodes right)

createHuffmanTree :: [(Weight, HuffmanTree Char)] -> HuffmanTree Char
createHuffmanTree [(_, tree)] = tree
createHuffmanTree xs =
    createHuffmanTree . constructSubTree $ xs
    where
        sortOnFrequency = Data.List.sortOn fst
        constructSubTree list =
            combine (take 2 sortedList) : drop 2 sortedList
            where sortedList = sortOnFrequency list
        combine [(w1,node1),(w2,node2)] = (w1 + w2, InternalNode node1 node2)
        combine [node] = node
