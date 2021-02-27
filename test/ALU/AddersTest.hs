module ALU.AddersTest
    ( test_adders
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import ALU.Adders

-- tests
test_adders :: TestTree
test_adders =
    testGroup
        "ALU.Adders"
        [ testGroup "halfAdder"
            [ testCase "0 + 0" (assertEqual "0, 0" (_0, _0) (halfAdder _0 _0))
            , testCase "0 + 1" (assertEqual "1, 0" (_1, _0) (halfAdder _0 _1))
            , testCase "1 + 0" (assertEqual "1, 0" (_1, _0) (halfAdder _1 _0))
            , testCase "1 + 1" (assertEqual "0, 1" (_0, _1) (halfAdder _1 _1))
            ]
        , testGroup "fullAdder"
            [ testCase "0 + 0 + 0" (assertEqual "0, 0" (_0, _0) (fullAdder _0 _0 _0))
            , testCase "0 + 0 + 1" (assertEqual "1, 0" (_1, _0) (fullAdder _0 _0 _1))
            , testCase "0 + 1 + 0" (assertEqual "1, 0" (_1, _0) (fullAdder _0 _1 _0))
            , testCase "0 + 1 + 1" (assertEqual "0, 1" (_0, _1) (fullAdder _0 _1 _1))
            , testCase "1 + 0 + 0" (assertEqual "1, 0" (_1, _0) (fullAdder _1 _0 _0))
            , testCase "1 + 0 + 1" (assertEqual "0, 1" (_0, _1) (fullAdder _1 _0 _1))
            , testCase "1 + 1 + 0" (assertEqual "0, 1" (_0, _1) (fullAdder _1 _1 _0))
            , testCase "1 + 1 + 1" (assertEqual "1, 1" (_1, _1) (fullAdder _1 _1 _1))
            ]
        , testGroup "add16"
            [ testCase "5 + 5" (assertEqual "10" (bus16 10) (add16 (bus16 5) (bus16 5)))
            , testCase "29 + 48" (assertEqual "77" (bus16 77) (add16 (bus16 29) (bus16 48)))
            , testCase "-1 + -1" (assertEqual "-2" (bus16 (-2)) (add16 (bus16 (-1)) (bus16 (-1))))
            , testCase "-10 + -38" (assertEqual "-48" (bus16 (-48)) (add16 (bus16 (-10)) (bus16 (-38))))
            ]
        , testGroup "inc16"
            [ testCase "increment 5" (assertEqual "6" (bus16 6) (inc16 (bus16 5)))
            ]
        ]
