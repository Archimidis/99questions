module Main where

import Test.Tasty (defaultMain, testGroup)

import Questions1to10.Test

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [Questions1to10.Test.tests]
