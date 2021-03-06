module Questions46to50.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

import TestUtils (testWithProvider)

import Questions46to50

tests :: TestTree
tests = testGroup "Questions 46 to 50" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    []

units :: TestTree
units = testGroup "Units"
    [ testWithProvider "table" testTable examplesForTable
    , testWithProvider "tablen" testTablen examplesForTablen
    , testWithProvider "gray" testGray examplesForGray
    , testWithProvider "huffman" testHuffman examplesForHuffman
    , testWithProvider "Huffman creation" testHuffmanTreeCreation examplesForHuffmanTree
    ]

testTable :: (Bool -> Bool -> Bool, [[Bool]]) -> Assertion
testTable (lambda, expected) =
    Questions46to50.table lambda @?= expected

examplesForTable =
    [
        (
            and',
            [
                [True, True, True],
                [True, False, False],
                [False, True, False],
                [False, False, False]
            ]
        ),
        (
            \a b -> (and' a (or' a b)),
            [
                [True, True, True],
                [True, False, True],
                [False, True, False],
                [False, False, False]
            ]
        ),
        (
            \a b -> (impl' a (equ' a b)),
            [
                [True, True, True],
                [True, False, False],
                [False, True, True],
                [False, False, True]
            ]
        ),
        (
            \a b -> (a `and'` (a `or'` not b)),
            [
                [True, True, True],
                [True, False, True],
                [False, True, False],
                [False, False, False]
            ]
        )
    ]

testTablen :: (Int, [Bool] -> Bool, [[Bool]]) -> Assertion
testTablen (n, lambda, expected) =
    Questions46to50.tablen n lambda @?= expected

examplesForTablen =
    [
        (
            3,
            \[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c,
            [
                [True,  True,  True,  True],
                [True,  True,  False, True],
                [True,  False, True,  True],
                [True,  False, False, False],
                [False, True,  True,  False],
                [False, True,  False, False],
                [False, False, True,  False],
                [False, False, False, False]
            ]
        )
    ]

testGray :: (Int, [String]) -> Assertion
testGray (n, expected) =
    Questions46to50.gray n @?= expected

examplesForGray :: [(Int, [String])]
examplesForGray =
    [
        (1,  ["0","1"]),
        (2,  ["00","01","11","10"]),
        (3,  ["000","001","011","010","110","111","101","100"])
    ]

testHuffman :: ([(Symbol, Weight)], [(Symbol, Code)]) -> Assertion
testHuffman (input, expected) =
    Questions46to50.huffman input @?= expected

examplesForHuffman :: [([(Symbol, Weight)], [(Symbol, Code)])]
examplesForHuffman =
    [
        (
            [],
            []
        ),
        (
            [('a', 1)],
            [('a', "")]
        ),
        (
            [('a', 0.5), ('b', 0.5)],
            [('a', "0"),('b', "1")]
        ),
        (
            [('a', 0.1), ('b', 0.3), ('c', 0.7)],
            [('a', "00"),('b', "01"),('c',"1")]
        ),
        (
            [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)],
            [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
        )
    ]

testHuffmanTreeCreation :: ([(Weight, HuffmanTree Char)], HuffmanTree Char) -> Assertion
testHuffmanTreeCreation (input, expected) =
    Questions46to50.createHuffmanTree input @?= expected

examplesForHuffmanTree :: [([(Weight, HuffmanTree Char)], HuffmanTree Char)]
examplesForHuffmanTree =
    [
        (
            [(0.5, Leaf 'a')],
            Leaf 'a'
        ),
        (
            [(0.5, Leaf 'a'), (0.5, Leaf 'b')],
            InternalNode (Leaf 'a') (Leaf 'b')
        ),
        (
            [(0.1, Leaf 'a'), (0.3, Leaf 'b'), (0.7, Leaf 'c')],
            InternalNode
                (InternalNode (Leaf 'a') (Leaf 'b'))
                (Leaf 'c')
        ),
        (
            [(0.03, Leaf 'a'), (0.07, Leaf 'b'), (0.3, Leaf 'c'), (0.6, Leaf 'd')],
            InternalNode
                (InternalNode
                    (InternalNode (Leaf 'a') (Leaf 'b'))
                    (Leaf 'c'))
                (Leaf 'd')
        ),
        (
            [(0.3, Leaf 'c'), (0.07, Leaf 'b'), (0.6, Leaf 'd'), (0.03, Leaf 'a')],
            InternalNode
                (InternalNode
                    (InternalNode (Leaf 'a') (Leaf 'b'))
                    (Leaf 'c'))
                (Leaf 'd')
        )
    ]
