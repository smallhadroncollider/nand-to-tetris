module UtilTest
    ( test_util
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util

-- tests
test_util :: TestTree
test_util =
    testGroup
        "Util"
        [ testGroup "toBinaryText"
            [ testCase "0" (assertEqual "00" "00" (toBinaryText 0 2))
            , testCase "1" (assertEqual "01" "01" (toBinaryText 1 2))
            , testCase "1" (assertEqual "11" "11" (toBinaryText (-1) 2))
            , testCase "1" (assertEqual "10" "10" (toBinaryText (-2) 2))
            , testCase "0" (assertEqual "0" "0000000000000000" (toBinaryText 0 16))
            , testCase "1" (assertEqual "1" "0000000000000001" (toBinaryText 1 16))
            , testCase "-1" (assertEqual "-1" "1111111111111111" (toBinaryText (-1) 16))
            , testCase "-1" (assertEqual "-2" "1111111111111110" (toBinaryText (-2) 16))
            ]
        ]
