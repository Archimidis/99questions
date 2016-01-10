module Main where

import Test.Tasty (defaultMain, testGroup)

import Questions1to10.Test
import Questions11to20.Test
import Questions21to28.Test
import Questions31to41.Test
import Questions46to50.Test
import Questions54Ato60.Test

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Questions1to10.Test.tests
    , Questions11to20.Test.tests
    , Questions21to28.Test.tests
    , Questions31to41.Test.tests
    , Questions46to50.Test.tests
    , Questions54Ato60.Test.tests
    ]
