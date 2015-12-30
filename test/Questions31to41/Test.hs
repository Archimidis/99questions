module Questions31to41.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

import TestUtils (testWithProvider)

import Questions31to41

tests :: TestTree
tests = testGroup "Questions 31 to 41" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "prime factors" propPrimeFactors
    , QC.testProperty "prime factors with multiplicity" propPrimeFactorsMutl ]


propPrimeFactors n =
    n > 1 ==> product (Questions31to41.primeFactors n) == n

propPrimeFactorsMutl n =
    n > 1 ==>
        (product . map (uncurry (^)) . Questions31to41.primeFactorsMult $ n) == n

units :: TestTree
units = testGroup "Units"
    [ testWithProvider "isPrime" testIsPrime examplesForIsPrime
    , testWithProvider "isPrime (optimization)" testIsPrimeV2 examplesForIsPrime
    , testWithProvider "myGCD" testMyGCD examplesForMyGCD
    , testCase "coprime" (Questions31to41.coprime 35 64 @?= True)
    , testCase "totient of m" (Questions31to41.totient 10 @?= 4)
    , testCase "totient of 1" (Questions31to41.totient 1 @?= 1)
    , testCase "primeFactors" (Questions31to41.primeFactors 315 @?= [3,3,5,7])
    , testCase "primeFactorsMult" 
        (Questions31to41.primeFactorsMult 315 @?= [(3,2),(5,1),(7,1)])
    , testCase "primeFactorsMult" 
        (Questions31to41.primeFactorsMult 6 @?= [(2,1),(3,1)])
    , testCase "totientImproved of m" (Questions31to41.totientImproved 10 @?= 4)
    , testCase "totientImproved of 1" (Questions31to41.totientImproved 1 @?= 1)
    , testCase "primesR" (Questions31to41.primesR 10 20 @?= [11,13,17,19])
    , testCase "goldbach" (Questions31to41.goldbach 28 @?= (5, 23))
    , testCase "goldbachList" 
        (Questions31to41.goldbachList 9 20 @?= [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)])
    ]

testIsPrime :: (Int, Bool) -> Assertion
testIsPrime (n, expected) =
    Questions31to41.isPrime n @?= expected

testIsPrimeV2 :: (Int, Bool) -> Assertion
testIsPrimeV2 (n, expected) =
    Questions31to41.isPrimeV2 n @?= expected

examplesForIsPrime :: [(Int, Bool)]
examplesForIsPrime =
    [
        (1, False),
        (2, True),
        (3, True),
        (4, False),
        (5, True),
        (6, False),
        (7, True),
        (8, False),
        (11, True),
        (13, True),
        (17, True),
        (19, True),
        (23, True),
        (25, False),
        (29 , True)
    ]

testMyGCD :: (Int, Int, Int) -> Assertion
testMyGCD (a, b, expected) =
    Questions31to41.myGCD a b @?= expected

examplesForMyGCD :: [(Int, Int, Int)]
examplesForMyGCD =
    [
        (36, 63, 9),
        (-3, -6, 3),
        (-3, 6, 3)
    ]
