module TestUtils (testWithProvider) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion)

testWithProvider :: String -> (a -> Assertion) -> [a] -> TestTree
testWithProvider testGroupName testFunction =
    testGroup testGroupName . map createTest . zipWith assignName [1::Int ..]
    where
        createTest (name, dataSet) = testCase name $ testFunction dataSet
        assignName setNumber dataSet = ("Data set " ++ show setNumber, dataSet)

