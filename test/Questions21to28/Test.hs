module Questions21to28.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

import TestUtils (testWithProvider)

import Questions21to28

tests :: TestTree
tests = testGroup "Questions 21 to 28" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    []

units :: TestTree
units = testGroup "Units"
    [ testWithProvider "insertAt (char into string)" testInsertAt examplesForInsertAt
    , testWithProvider "insertAt (string into string)" testInsertListAt examplesForInsertListAt
    , testWithProvider "insertAt (string into string)" testRange examplesForRange
    , testWithProvider "combinations" testCombinations examplesForCombinations
    ]

testInsertAt :: (Char, String, Int, String) -> Assertion
testInsertAt (char, string, place, expected) =
    Questions21to28.insertAt char string place @?= expected

examplesForInsertAt :: [(Char, String, Int, String)]
examplesForInsertAt =
    [
        ('a', "bb", -1, "abb"),
        ('a', "", 1, "a"),
        ('a', "bb", 1, "abb"),
        ('a', "bb", 3, "bba"),
        ('X', "abcd", 2, "aXbcd")
    ]

testInsertListAt :: (String, String, Int, String) -> Assertion
testInsertListAt (element, string, place, expected) =
    Questions21to28.insertListAt element string place @?= expected

examplesForInsertListAt :: [(String, String, Int, String)]
examplesForInsertListAt =
    [
        ("XX", "bb", -1, "XXbb"),
        ("XX", "", 1, "XX"),
        ("XX", "bb", 1, "XXbb"),
        ("XX", "bb", 3, "bbXX"),
        ("XX", "abcd", 2, "aXXbcd")
    ]

testRange :: (Int, Int, [Int]) -> Assertion
testRange (start, end, expected) =
    Questions21to28.range start end @?= expected

examplesForRange :: [(Int, Int, [Int])]
examplesForRange =
    [
        (0, 0, [0]),
        (0, 1, [0, 1]),
        (2, 6, [2..6]),
        (6, 2, [6,5,4,3,2])
    ]

testCombinations :: (Int, String, [String]) -> Assertion
testCombinations (k, list, expected) =
    Questions21to28.combinations k list @?= expected

examplesForCombinations :: [(Int, String, [String])]
examplesForCombinations =
    [
        (3, "", []),
        (0, "ab", []),
        (2, "ab", ["ab"]),
        (2, "abc", ["ab", "ac", "bc"]),
	(3, "abcd", ["abc", "abd", "acd", "bcd"])
    ]
