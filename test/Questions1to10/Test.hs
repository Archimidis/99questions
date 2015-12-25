module Questions1to10.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck as QC

import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

import Questions1to10

tests :: TestTree
tests = testGroup "Questions 1 to 10" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "find last of list" propFindLast ]

propFindLast :: [Int] -> Property
propFindLast xs = 
    not (null xs) ==>
        Questions1to10.myLast xs == last xs

units :: TestTree
units = testGroup "Units"
    [ testCase "find last in int list" testFindLastIntListElement
    , testCase "find last in string" testFindLastCharListElement
    , testCase "find last but one in int list" testFindLastButOneIntListElement
    , testCase "find last but one in string" testFindLastButOneCharListElement
    , testCase "find k element in int list" testFindKElementOfIntList
    , testCase "find k element in string" testFindKElementOfCharList
    , testCase "find length of empty list" testFindLengthOfEmptyList
    , testCase "find length of int list" testFindLengthOfIntList
    , testCase "find length of string" testFindLengthOfCharList
    , testCase "reverse empty list" testReverseEmptyList
    , testCase "reverse string" testReverseString
    , testCase "reverse int list" testReverseIntList
    , testCase "palindome int list" testIntListThatIsNotPalindrome
    , testCase "not palindome int list" testIntListThatIsPalindrome
    , testCase "not palindome string" testStringThatIsPalindrome
    , testCase "not palindome stintg" testStringThatIsNotPalindrome
    , testCase "flatten empty list" testFlattenEmptyList
    , testCase "flatten single element" testFlattenSingleElement
    , testCase "flatten nested list" testFlattenList
    , testCase "compress an empty list" testCompressEmptyList
    , testCase "compress list with same chars" testCompressListWithSameChars
    , testCase "pack empty list" testPackEmptyList
    , testCase "pack list \"aa\"" testPackListWith2ConsecutiveSameChars
    , testCase "pack list \"aab\"" testPackListWith2ConsecutiveSameAnd1Diff
    , testCase "pack list \"aabba\"" testPackListWith2PairsConsecutiveSameAnd1DiffChars
    , testCase "pack list \"aaaabccaadeeee\"" testPackListWithComplexInput
    , testCase "encode \"aaaabccaadeeee\"" testEncode
    ]

testFindLastIntListElement :: Assertion
testFindLastIntListElement =
    Questions1to10.myLast [1,2,3,4] @?= 4

testFindLastCharListElement :: Assertion
testFindLastCharListElement =
    Questions1to10.myLast "xyz" @?= 'z'

testFindLastButOneIntListElement :: Assertion
testFindLastButOneIntListElement =
    Questions1to10.myButLast [1,2,3,4] @?= 3

testFindLastButOneCharListElement :: Assertion
testFindLastButOneCharListElement =
    Questions1to10.myButLast "xyz" @?= 'y'

testFindKElementOfIntList :: Assertion
testFindKElementOfIntList =
    Questions1to10.elementAt [1,2,3] 1 @?= 1

testFindKElementOfCharList :: Assertion
testFindKElementOfCharList =
    Questions1to10.elementAt "haskell" 7 @?= 'l'

testFindLengthOfEmptyList :: Assertion
testFindLengthOfEmptyList =
    Questions1to10.myLength [] @?= 0

testFindLengthOfIntList :: Assertion
testFindLengthOfIntList =
    Questions1to10.myLength [1,2,3] @?= 3

testFindLengthOfCharList :: Assertion
testFindLengthOfCharList =
    Questions1to10.myLength "Hello, world!" @?= 13

testReverseEmptyList :: Assertion
testReverseEmptyList =
    Questions1to10.myReverse ([] :: [Int]) @?= ([] :: [Int])

testReverseString :: Assertion
testReverseString =
    Questions1to10.myReverse "hello" @?= "olleh"

testReverseIntList :: Assertion
testReverseIntList =
    Questions1to10.myReverse [1,2,3,4] @?= [4,3,2,1]

testIntListThatIsPalindrome :: Assertion
testIntListThatIsPalindrome =
    Questions1to10.isPalindrome [1,2,1] @?= True

testIntListThatIsNotPalindrome :: Assertion
testIntListThatIsNotPalindrome =
    Questions1to10.isPalindrome [1,2,3] @?= False

testStringThatIsPalindrome :: Assertion
testStringThatIsPalindrome =
    Questions1to10.isPalindrome "madam" @?= True

testStringThatIsNotPalindrome :: Assertion
testStringThatIsNotPalindrome =
    Questions1to10.isPalindrome "not palindrome" @?= False

testFlattenEmptyList :: Assertion
testFlattenEmptyList =
    Questions1to10.flatten (List []) @?= ([] :: [Int])

testFlattenSingleElement :: Assertion
testFlattenSingleElement =
    Questions1to10.flatten (Elem 5) @?= [5]

testFlattenList :: Assertion
testFlattenList =
    Questions1to10.flatten (List [List [Elem 1], List [Elem 2, List [Elem 3, Elem 4]]]) @?= [1,2,3,4]

testCompressEmptyList :: Assertion
testCompressEmptyList =
    Questions1to10.compress [] @?= ([] :: [Int])

testCompressListWithSameChars :: Assertion
testCompressListWithSameChars =
    Questions1to10.compress "aaaabccaadeeee" @?= "abcade"

testPackEmptyList :: Assertion
testPackEmptyList =
    Questions1to10.pack [] @?= ([] :: [String])

testPackListWith2ConsecutiveSameChars :: Assertion
testPackListWith2ConsecutiveSameChars =
    Questions1to10.pack "aa" @?= ["aa"]

testPackListWith2ConsecutiveSameAnd1Diff :: Assertion
testPackListWith2ConsecutiveSameAnd1Diff =
    Questions1to10.pack "aab" @?= ["aa", "b"]

testPackListWith2PairsConsecutiveSameAnd1DiffChars :: Assertion
testPackListWith2PairsConsecutiveSameAnd1DiffChars =
    Questions1to10.pack "aabba" @?= ["aa", "bb", "a"]

testPackListWithComplexInput :: Assertion
testPackListWithComplexInput =
    Questions1to10.pack "aaaabccaadeeee" @?= ["aaaa","b","cc","aa","d","eeee"]

testEncode :: Assertion
testEncode =
    Questions1to10.encode "aaaabccaadeeee" @?= [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
