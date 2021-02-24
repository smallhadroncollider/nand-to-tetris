module BusGatesTest
    ( test_bus_gates
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util (bit, bus8, bus16)
import Bus.Gates

-- tests
test_bus_gates :: TestTree
test_bus_gates =
    testGroup
        "BusGates"
        [ testGroup "Bus16"
            [ testGroup "not16"
                [ testCase "all 0s" (assertEqual "all 1s" (bus16 "1111111111111111") (not16 (bus16 "0000000000000000")))
                ]
            , testGroup "and16"
                [ testCase "all 0s" (assertEqual "all 0s" (bus16 "0000000000000000") (bus16 "0000000000000000" `and16` bus16 "0000000000000000"))
                , testCase "some 1s" (assertEqual "all 0s" (bus16 "0000000000000000") (bus16 "1000000000000001" `and16` bus16 "0000000000000000"))
                , testCase "some 1s" (assertEqual "some 1s" (bus16 "1000000000000001") (bus16 "1000000000000001" `and16` bus16 "1000000000000001"))
                , testCase "all 1s" (assertEqual "all 1s" (bus16 "1111111111111111") (bus16 "1111111111111111" `and16` bus16 "1111111111111111"))
                ]
            , testGroup "or16"
                [ testCase "all 0s" (assertEqual "all 0s" (bus16 "0000000000000000") (bus16 "0000000000000000" `or16` bus16 "0000000000000000"))
                , testCase "some 1s" (assertEqual "some 1s" (bus16 "1000000000000001") (bus16 "1000000000000000" `or16` bus16 "0000000000000001"))
                , testCase "all 1s" (assertEqual "all 1s" (bus16 "1111111111111111") (bus16 "1111111111111111" `or16` bus16 "1111111111111111"))
                ]
            , testGroup "mux16"
                [ testCase "all 0s" (assertEqual "all 0s" (bus16 "0000000000000000") (mux16 (bus16 "0000000000000000") (bus16 "1111111111111111") (bit "0")))
                , testCase "all 0s" (assertEqual "all 0s" (bus16 "1111111111111111") (mux16 (bus16 "0000000000000000") (bus16 "1111111111111111") (bit "1")))
                ]
            ]
        , testGroup "Bus8"
            [ testGroup "or8Way"
                [ testCase "all 0s" (assertEqual "0" (bit "0") (or8Way (bus8 "00000000")))
                , testCase "some 1s" (assertEqual "1" (bit "1") (or8Way (bus8 "00001000")))
                , testCase "some 1s" (assertEqual "1" (bit "1") (or8Way (bus8 "00000001")))
                , testCase "all 1s" (assertEqual "1" (bit "1") (or8Way (bus8 "11111111")))
                ]
            ]
        , testGroup "Multiplexors"
            [ testGroup "mux4Way16"
                [ testCase "first" (assertEqual "first input" (bus16 "0000000000000000") (mux4Way16 (bus16 "0000000000000000") (bus16 "0000000000000001") (bus16 "0000000000000010") (bus16 "0000000000000011") (bit "0") (bit "0")))
                , testCase "second" (assertEqual "second input" (bus16 "0000000000000001") (mux4Way16 (bus16 "0000000000000000") (bus16 "0000000000000001") (bus16 "0000000000000010") (bus16 "0000000000000011") (bit "0") (bit "1")))
                , testCase "third" (assertEqual "third input" (bus16 "0000000000000010") (mux4Way16 (bus16 "0000000000000000") (bus16 "0000000000000001") (bus16 "0000000000000010") (bus16 "0000000000000011") (bit "1") (bit "0")))
                , testCase "fourth" (assertEqual "fourth input" (bus16 "0000000000000011") (mux4Way16 (bus16 "0000000000000000") (bus16 "0000000000000001") (bus16 "0000000000000010") (bus16 "0000000000000011") (bit "1") (bit "1")))
                ]
            , testGroup "mux8Way16"
                [ testCase "first"
                    (assertEqual "first input" (bus16 "0000000000000000")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "0") (bit "0") (bit "0")
                        )
                    )
                , testCase "second"
                    (assertEqual "second input" (bus16 "0000000000000001")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "0") (bit "0") (bit "1")
                        )
                    )
                , testCase "third"
                    (assertEqual "third input" (bus16 "0000000000000010")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "0") (bit "1") (bit "0")
                        )
                    )
                , testCase "fourth"
                    (assertEqual "fourth input" (bus16 "0000000000000011")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "0") (bit "1") (bit "1")
                        )
                    )
                , testCase "fifth"
                    (assertEqual "fifth input" (bus16 "0000000000000100")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "1") (bit "0") (bit "0")
                        )
                    )
                , testCase "sixth"
                    (assertEqual "sixth input" (bus16 "0000000000000101")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "1") (bit "0") (bit "1")
                        )
                    )
                , testCase "seventh"
                    (assertEqual "seventh input" (bus16 "0000000000000110")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "1") (bit "1") (bit "0")
                        )
                    )
                , testCase "eigth"
                    (assertEqual "eigth input" (bus16 "0000000000000111")
                        (mux8Way16
                            (bus16 "0000000000000000")
                            (bus16 "0000000000000001")
                            (bus16 "0000000000000010")
                            (bus16 "0000000000000011")
                            (bus16 "0000000000000100")
                            (bus16 "0000000000000101")
                            (bus16 "0000000000000110")
                            (bus16 "0000000000000111")
                            (bit "1") (bit "1") (bit "1")
                        )
                    )
                ]
            ]
        ]
