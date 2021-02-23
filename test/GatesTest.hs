module GatesTest
    ( test_gates
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util.Binary (bit)
import Gates

-- tests
test_gates :: TestTree
test_gates =
    testGroup
        "Gates"
        [ testGroup "nand"
            [ testCase "0 0" (assertEqual "1" (bit "1") (bit "0" `nand` bit "0"))
            , testCase "0 1" (assertEqual "1" (bit "1") (bit "0" `nand` bit "1"))
            , testCase "1 0" (assertEqual "1" (bit "1") (bit "1" `nand` bit "0"))
            , testCase "1 1" (assertEqual "0" (bit "0") (bit "1" `nand` bit "1"))
            ]
        , testGroup "not"
            [ testCase "0" (assertEqual "1" (bit "1") (not (bit "0")))
            , testCase "1" (assertEqual "0" (bit "0") (not (bit "1")))
            ]
        , testGroup "and"
            [ testCase "0 0" (assertEqual "0" (bit "0") (bit "0" `and` bit "0"))
            , testCase "0 1" (assertEqual "0" (bit "0") (bit "0" `and` bit "1"))
            , testCase "1 0" (assertEqual "0" (bit "0") (bit "1" `and` bit "0"))
            , testCase "1 1" (assertEqual "1" (bit "1") (bit "1" `and` bit "1"))
            ]
        , testGroup "or"
            [ testCase "0 0" (assertEqual "0" (bit "0") (bit "0" `or` bit "0"))
            , testCase "0 1" (assertEqual "1" (bit "1") (bit "0" `or` bit "1"))
            , testCase "1 0" (assertEqual "1" (bit "1") (bit "1" `or` bit "0"))
            , testCase "1 1" (assertEqual "1" (bit "1") (bit "1" `or` bit "1"))
            ]
        , testGroup "xor"
            [ testCase "0 0" (assertEqual "0" (bit "0") (bit "0" `xor` bit "0"))
            , testCase "0 1" (assertEqual "1" (bit "1") (bit "0" `xor` bit "1"))
            , testCase "1 0" (assertEqual "1" (bit "1") (bit "1" `xor` bit "0"))
            , testCase "1 1" (assertEqual "0" (bit "0") (bit "1" `xor` bit "1"))
            ]

        , testGroup "mux"
            [ testCase "0 0 0" (assertEqual "0" (bit "0") (mux (bit "0") (bit "0") (bit "0")))
            , testCase "0 1 0" (assertEqual "0" (bit "0") (mux (bit "0") (bit "1") (bit "0")))
            , testCase "1 0 0" (assertEqual "1" (bit "1") (mux (bit "1") (bit "0") (bit "0")))
            , testCase "1 1 0" (assertEqual "1" (bit "1") (mux (bit "1") (bit "1") (bit "0")))
            , testCase "0 0 1" (assertEqual "0" (bit "0") (mux (bit "0") (bit "0") (bit "1")))
            , testCase "0 1 1" (assertEqual "1" (bit "1") (mux (bit "0") (bit "1") (bit "1")))
            , testCase "1 0 1" (assertEqual "0" (bit "0") (mux (bit "1") (bit "0") (bit "1")))
            , testCase "1 1 1" (assertEqual "1" (bit "1") (mux (bit "1") (bit "1") (bit "1")))
            ]

        , testGroup "dmux"
            [ testCase "0 0" (assertEqual "0" (bit "0", bit "0") (dmux (bit "0") (bit "0")))
            , testCase "0 1" (assertEqual "0" (bit "1", bit "0") (dmux (bit "0") (bit "1")))
            , testCase "1 0" (assertEqual "1" (bit "0", bit "0") (dmux (bit "1") (bit "0")))
            , testCase "1 1" (assertEqual "1" (bit "0", bit "1") (dmux (bit "1") (bit "1")))
            ]
        ]
