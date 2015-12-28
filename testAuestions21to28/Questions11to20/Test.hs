module Questions11to20.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

import TestUtils (testWithProvider)

import Questions11to20

tests :: TestTree
tests = testGroup "Questions 11 to 20" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    []

units :: TestTree
units = testGroup "Units"
    [ testWithProvider "encodeModified" testEncodeModified examplesForEncode
    , testWithProvider "testDecode" testDecode exapmlesForDecode
    , testWithProvider "encodeDirect" testEncodeDirect examplesForEncode
    , testWithProvider "dupli" testDuplicate examplesForDuplicate
    , testWithProvider "repli" testReplicate examplesForReplicate
    , testWithProvider "drop" testDropEvery examplesForDropEvery
    , testWithProvider "split" testSplit examplesForSplit
    , testWithProvider "split (variant 2)" testSplitV2 examplesForSplit
    , testWithProvider "split (variant 3)" testSplitV3 examplesForSplit
    , testWithProvider "split (variant 4)" testSplitV4 examplesForSplit
    , testWithProvider "slice" testSlice examplesForSlice
    , testWithProvider "rotate" testRotate examplesForRotate
    , testWithProvider "removeAt" testRemoveAt examplesForRemoveAt
    , testWithProvider "removeAt (point free)" testRemoveAtv2 examplesForRemoveAt
    ]

testEncodeModified :: (String, [EncodedValue Char]) -> Assertion
testEncodeModified (input, expected) =
    Questions11to20.encodeModified input @?= expected

testEncodeDirect :: (String, [EncodedValue Char]) -> Assertion
testEncodeDirect (input, expected) =
    Questions11to20.encodeDirect input @?= expected

examplesForEncode :: [(String, [EncodedValue Char])]
examplesForEncode =
    [
        ("a", [Single 'a']),
        ("aa", [Multiple 2 'a']),
        ("aaa", [Multiple 3 'a']),
        ("ab", [Single 'a', Single 'b']),
        ("aab", [Multiple 2 'a', Single 'b']),
        ("aabcaa", [Multiple 2 'a', Single 'b', Single 'c', Multiple 2 'a']),
        ("aaaabccaadeeee", [Multiple 4 'a', Single 'b', Multiple 2 'c',
            Multiple 2 'a', Single 'd', Multiple 4 'e'])
    ]

testDecode :: ([EncodedValue Char], String) -> Assertion
testDecode (input, expected) =
    Questions11to20.decodeModified input @?= expected

exapmlesForDecode :: [([EncodedValue Char], String)]
exapmlesForDecode =
    [
        ([], ""),
        ([Single 'a'], "a"),
        ([Multiple 2 'a'], "aa"),
        ([Single 'a', Single 'b'], "ab"),
        ([Multiple 2 'a', Single 'b'], "aab"),
        ([Multiple 4 'a', Single 'b', Multiple 2 'c',
            Multiple 2 'a', Single 'd', Multiple 4 'e'], "aaaabccaadeeee")
    ]

testDuplicate :: ([Int], [Int]) -> Assertion
testDuplicate (input, expected) =
    Questions11to20.dupli input @?= expected

examplesForDuplicate :: [([Int], [Int])]
examplesForDuplicate =
    [
        ([], []),
        ([1], [1,1]),
        ([1,2], [1,1,2,2]),
        ([1,2,3], [1,1,2,2,3,3])
    ]

testReplicate :: ([Int], Int, [Int]) -> Assertion
testReplicate (list, n, expected) =
    Questions11to20.repli list n @?= expected

examplesForReplicate :: [([Int], Int, [Int])]
examplesForReplicate =
    [
        ([], 10, []),
        ([1], 1, [1]),
        ([1], 3, [1,1,1]),
        ([1,2], 3, [1,1,1,2,2,2]),
        ([1,2,3], 2, [1,1,2,2,3,3])
    ]

testDropEvery :: ([Int], Int, [Int]) -> Assertion
testDropEvery (list, n, expected) =
    Questions11to20.dropEvery list n @?= expected

examplesForDropEvery :: [([Int], Int, [Int])]
examplesForDropEvery =
    [
        ([], 10, []),
        ([1], 2, [1]),
        ([1,2,3], 3, [1,2]),
        ([1..6], 2, [1,3,5])
    ]

testSplit :: ([Int], Int, ([Int], [Int])) -> Assertion
testSplit (list, n, expected) =
    Questions11to20.split list n @?= expected

testSplitV2 :: ([Int], Int, ([Int], [Int])) -> Assertion
testSplitV2 (list, n, expected) =
    Questions11to20.splitv2 list n @?= expected

testSplitV3 :: ([Int], Int, ([Int], [Int])) -> Assertion
testSplitV3 (list, n, expected) =
    Questions11to20.splitv3 list n @?= expected

testSplitV4 :: ([Int], Int, ([Int], [Int])) -> Assertion
testSplitV4 (list, n, expected) =
    Questions11to20.splitv4 list n @?= expected

examplesForSplit :: [([Int], Int, ([Int], [Int]))]
examplesForSplit =
    [
        ([], 10, ([],[])),
        ([1], 2, ([1],[])),
        ([1,2,3], 2, ([1,2], [3])),
        ([1..6], 2, ([1,2], [3..6]))
    ]

testSlice :: (String, Int, Int, String) -> Assertion
testSlice (list, i, k, expected) =
    Questions11to20.slice list i k @?= expected

examplesForSlice :: [(String, Int, Int, String)]
examplesForSlice =
    [
        ("", 1, 2, ""),
        ("a", 1, 2, "a"),
        ("a", 3, 5, ""),
        ("ab", 1, 1, "a"),
        ("ab", 2, 1, ""),
        ("ab", 1, 2, "ab"),
        ("abcd", 1, 2, "ab"),
        (['a'..'k'], 3, 7, ['c'..'g'])
    ]

testRotate :: (String, Int, String) -> Assertion
testRotate (list, n, expected) =
    Questions11to20.rotate list n @?= expected

examplesForRotate :: [(String, Int, String)]
examplesForRotate =
    [
        ("", 1, ""),
        ("a", 2, "a"),
        ("ab", 1, "ba"),
        (['a'..'d'], 2, "cdab"),
        ("abc", -1, "cab"),
        (['a'..'h'], 3, "defghabc"),
        (['a'..'h'], -2, "ghabcdef"),
        ("ab", 0, "ab"),
        ("ab", 2, "ab"),
        ("ab", 3, "ba")
    ]

testRemoveAt :: (Int, String, (Char, String)) -> Assertion
testRemoveAt (list, n, expected) =
    Questions11to20.removeAt list n @?= expected

testRemoveAtv2 :: (Int, String, (Char, String)) -> Assertion
testRemoveAtv2 (list, n, expected) =
    Questions11to20.removeAtv2 list n @?= expected

examplesForRemoveAt :: [(Int, String, (Char, String))]
examplesForRemoveAt =
    [
        (1, "a", ('a', "")),
        (1, "ab", ('a', "b")),
        (2, ['a'..'d'], ('b', "acd"))
    ]
