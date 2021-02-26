module Sequential.DFFTest
    ( test_dff
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.State.Strict (runState)

import Util
import Sequential.DFF

-- tests
test_dff :: TestTree
test_dff =
    testGroup
        "Sequential.DFF"
        [ testGroup "dff"
            [ testCase "" (assertEqual "" (bit "0", bit "0") (runState (dff (bit "0")) (bit "0")))
            , testCase "" (assertEqual "" (bit "1", bit "0") (runState (dff (bit "0")) (bit "1")))
            , testCase "" (assertEqual "" (bit "0", bit "1") (runState (dff (bit "1")) (bit "0")))
            , testCase "" (assertEqual "" (bit "1", bit "1") (runState (dff (bit "1")) (bit "1")))
            ]
        ]
