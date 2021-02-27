module Sequential.DFFTest
    ( test_dff
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Sequential.DFF

-- tests
test_dff :: TestTree
test_dff =
    testGroup
        "Sequential.DFF"
        [ testGroup "dff"
            [ testCase "" (assertEqual "" (bit "0", bit "0") (state (dff (bit "0")) (bit "0")))
            , testCase "" (assertEqual "" (bit "1", bit "0") (state (dff (bit "0")) (bit "1")))
            , testCase "" (assertEqual "" (bit "0", bit "1") (state (dff (bit "1")) (bit "0")))
            , testCase "" (assertEqual "" (bit "1", bit "1") (state (dff (bit "1")) (bit "1")))
            ]
        ]
