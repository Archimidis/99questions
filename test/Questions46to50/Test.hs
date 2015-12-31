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
    [ testWithProvider "table" testTable exapmlesForTable
    , testWithProvider "tablen" testTablen exapmlesForTablen
    , testWithProvider "tablen" testGray examplesForGray
    ]

testTable :: (Bool -> Bool -> Bool, [[Bool]]) -> Assertion
testTable (lambda, expected) =
    Questions46to50.table lambda @?= expected

exapmlesForTable =
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

exapmlesForTablen =
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
