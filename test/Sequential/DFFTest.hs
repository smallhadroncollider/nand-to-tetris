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
            [ testCase "" (assertEqual "" (_0, _0) (state (dff _0) _0))
            , testCase "" (assertEqual "" (_1, _0) (state (dff _0) _1))
            , testCase "" (assertEqual "" (_0, _1) (state (dff _1) _0))
            , testCase "" (assertEqual "" (_1, _1) (state (dff _1) _1))
            ]
        ]
