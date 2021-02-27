module Bit.GatesTest
    ( test_gates
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Bit.Gates

-- tests
test_gates :: TestTree
test_gates =
    testGroup
        "Bit.Gates"
        [ testGroup "nand"
            [ testCase "0 0" (assertEqual "1" _1 (_0 `nand` _0))
            , testCase "0 1" (assertEqual "1" _1 (_0 `nand` _1))
            , testCase "1 0" (assertEqual "1" _1 (_1 `nand` _0))
            , testCase "1 1" (assertEqual "0" _0 (_1 `nand` _1))
            ]
        , testGroup "not"
            [ testCase "0" (assertEqual "1" _1 (not _0))
            , testCase "1" (assertEqual "0" _0 (not _1))
            ]
        , testGroup "and"
            [ testCase "0 0" (assertEqual "0" _0 (_0 `and` _0))
            , testCase "0 1" (assertEqual "0" _0 (_0 `and` _1))
            , testCase "1 0" (assertEqual "0" _0 (_1 `and` _0))
            , testCase "1 1" (assertEqual "1" _1 (_1 `and` _1))
            ]
        , testGroup "or"
            [ testCase "0 0" (assertEqual "0" _0 (_0 `or` _0))
            , testCase "0 1" (assertEqual "1" _1 (_0 `or` _1))
            , testCase "1 0" (assertEqual "1" _1 (_1 `or` _0))
            , testCase "1 1" (assertEqual "1" _1 (_1 `or` _1))
            ]
        , testGroup "xor"
            [ testCase "0 0" (assertEqual "0" _0 (_0 `xor` _0))
            , testCase "0 1" (assertEqual "1" _1 (_0 `xor` _1))
            , testCase "1 0" (assertEqual "1" _1 (_1 `xor` _0))
            , testCase "1 1" (assertEqual "0" _0 (_1 `xor` _1))
            ]

        , testGroup "mux"
            [ testCase "0 0 0" (assertEqual "0" _0 (mux _0 _0 _0))
            , testCase "0 1 0" (assertEqual "0" _0 (mux _0 _1 _0))
            , testCase "1 0 0" (assertEqual "1" _1 (mux _1 _0 _0))
            , testCase "1 1 0" (assertEqual "1" _1 (mux _1 _1 _0))
            , testCase "0 0 1" (assertEqual "0" _0 (mux _0 _0 _1))
            , testCase "0 1 1" (assertEqual "1" _1 (mux _0 _1 _1))
            , testCase "1 0 1" (assertEqual "0" _0 (mux _1 _0 _1))
            , testCase "1 1 1" (assertEqual "1" _1 (mux _1 _1 _1))
            ]

        , testGroup "dmux"
            [ testCase "0 0" (assertEqual "0" (_0, _0) (dmux _0 _0))
            , testCase "0 1" (assertEqual "0" (_1, _0) (dmux _0 _1))
            , testCase "1 0" (assertEqual "1" (_0, _0) (dmux _1 _0))
            , testCase "1 1" (assertEqual "1" (_0, _1) (dmux _1 _1))
            ]
        ]
