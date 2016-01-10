module Questions54Ato60.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

import TestUtils (testWithProvider)

import Questions54Ato60

tests :: TestTree
tests = testGroup "Questions 54A to 60" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "hbalTree construct height-balanced b-trees" propHbalTree]

{-keeping height low to speed-up tests-}
arbitraryHeight :: Gen Int
arbitraryHeight = choose (0, 4)

propHbalTree :: Char -> Property
propHbalTree e =
    forAll arbitraryHeight $ \height ->
        all btreeHeightBalanced $ hbalTree e height

units :: TestTree
units = testGroup "Units"
    [ testWithProvider "cbalTree" testCbalTree examplesForCbalTree
    , testWithProvider "symmetric" testSymmetric examplesForSymmetric
    , testWithProvider "construct" testConstruct examplesForConstruct
    , testWithProvider "symCbalTrees" testSymCbalTrees examplesForSymCbalTrees
    ]

testCbalTree :: (Int, [Tree Char]) -> Assertion
testCbalTree (input, expected) =
    Questions54Ato60.cbalTree input @?= expected

examplesForCbalTree :: [(Int, [Tree Char])]
examplesForCbalTree =
    [
        (0, [Empty]),
        (1, [leaf 'x']),
        (
            2,
            [
                Branch 'x' Empty (leaf 'x'),
                Branch 'x' (leaf 'x') Empty
            ]
        ),
        (
            4,
            [
                Branch 'x'
                    (leaf 'x')
                    (Branch 'x' Empty (leaf 'x')),
                Branch 'x'
                    (Branch 'x' Empty (leaf 'x'))
                    (leaf 'x'),
                Branch 'x'
                    (leaf 'x')
                    (Branch 'x' (leaf 'x') Empty),
                Branch 'x'
                    (Branch 'x' (leaf 'x') Empty)
                    (leaf 'x')
            ]
        )
    ]

testSymmetric :: (Tree Char, Bool) -> Assertion
testSymmetric (input, expected) =
    Questions54Ato60.symmetric input @?= expected

examplesForSymmetric :: [(Tree Char, Bool)]
examplesForSymmetric =
    [
        (Empty, True),
        (leaf 'x', True),
        (
            Branch 'x' (leaf 'x') Empty,
            False
        ),
        (
            Branch 'x' (leaf 'x') (leaf 'x'),
            True
        ),
        (
            Branch 'x'
                (Branch 'x' (leaf 'x') Empty)
                (leaf 'x'),
            False
        ),
        (
            Branch 'x'
                (Branch 'x' (leaf 'x') (leaf 'x'))
                (Branch 'x' (leaf 'x') (leaf 'x')),
            True
        )
    ]

testConstruct :: ([Int], Tree Int) -> Assertion
testConstruct (input, expected) =
    Questions54Ato60.construct input @?= expected

examplesForConstruct :: [([Int], Tree Int)]
examplesForConstruct =
    [
        ([], Empty),
        ([1], leaf 1),
        (
            [1,2,3,4,5],
            Branch 1
                Empty
                (Branch 2
                    Empty
                    (Branch 3
                        Empty
                        (Branch 4
                            Empty (leaf 5))))
        ),
        (
            [3,2,5,7,1],
            Branch 3
                (Branch 2 (leaf 1) Empty)
                (Branch 5 Empty (leaf 7))
        )
    ]

testSymCbalTrees :: (Int, [Tree Char]) -> Assertion
testSymCbalTrees (input, expected) =
    Questions54Ato60.symCbalTrees input @?= expected

examplesForSymCbalTrees :: [(Int, [Tree Char])]
examplesForSymCbalTrees =
    [
        (0, []),
        (1, [leaf 'x']),
        (2, []),
        (3, [Branch 'x' (leaf 'x') (leaf 'x')]),
        (5, [
            Branch 'x'
                (Branch 'x' Empty (leaf 'x'))
                (Branch 'x' (leaf 'x') Empty),
            Branch 'x'
                (Branch 'x' (leaf 'x') Empty)
                (Branch 'x' Empty (leaf 'x'))
        ])
    ]
