module BusGatesTest
    ( test_bus_gates
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Bus.Gates

-- number reference
-- ================
-- 255: 11111111
-- 32768: 1000000000000000
-- 32769: 1000000000000001
-- 65535: 1111111111111111

-- tests
test_bus_gates :: TestTree
test_bus_gates =
    testGroup
        "BusGates"
        [ testGroup "Bus16"
            [ testGroup "not16"
                [ testCase "all 0s" (assertEqual "all 1s" (bus16 65535) (not16 (bus16 0)))
                ]
            , testGroup "and16"
                [ testCase "all 0s" (assertEqual "all 0s" (bus16 0) (bus16 0 `and16` bus16 0))
                , testCase "some 1s" (assertEqual "all 0s" (bus16 0) (bus16 32769 `and16` bus16 0))
                , testCase "some 1s" (assertEqual "some 1s" (bus16 32769) (bus16 32769  `and16` bus16 32769))
                , testCase "all 1s" (assertEqual "all 1s" (bus16 65535) (bus16 65535 `and16` bus16 65535))
                ]
            , testGroup "or16"
                [ testCase "all 0s" (assertEqual "all 0s" (bus16 0) (bus16 0 `or16` bus16 0))
                , testCase "some 1s" (assertEqual "some 1s" (bus16 32769) (bus16 32768 `or16` bus16 1))
                , testCase "all 1s" (assertEqual "all 1s" (bus16 65535) (bus16 65535 `or16` bus16 65535))
                ]
            , testGroup "mux16"
                [ testCase "all 0s" (assertEqual "all 0s" (bus16 0) (mux16 (bus16 0) (bus16 65535) (bit "0")))
                , testCase "all 0s" (assertEqual "all 0s" (bus16 65535) (mux16 (bus16 0) (bus16 65535) (bit "1")))
                ]
            ]
        , testGroup "Bus8"
            [ testGroup "or8Way"
                [ testCase "all 0s" (assertEqual "0" (bit "0") (or8Way (bus8 0)))
                , testCase "some 1s" (assertEqual "1" (bit "1") (or8Way (bus8 8)))
                , testCase "some 1s" (assertEqual "1" (bit "1") (or8Way (bus8 1)))
                , testCase "all 1s" (assertEqual "1" (bit "1") (or8Way (bus8 255)))
                ]
            ]
        , testGroup "Multiplexors"
            [ testGroup "mux4Way16"
                [ testCase "first" (assertEqual "first input" (bus16 0) (mux4Way16 (bus16 0) (bus16 1) (bus16 2) (bus16 3) (bit "0") (bit "0")))
                , testCase "second" (assertEqual "second input" (bus16 1) (mux4Way16 (bus16 0) (bus16 1) (bus16 2) (bus16 3) (bit "0") (bit "1")))
                , testCase "third" (assertEqual "third input" (bus16 2) (mux4Way16 (bus16 0) (bus16 1) (bus16 2) (bus16 3) (bit "1") (bit "0")))
                , testCase "fourth" (assertEqual "fourth input" (bus16 3) (mux4Way16 (bus16 0) (bus16 1) (bus16 2) (bus16 3) (bit "1") (bit "1")))
                ]
            , testGroup "mux8Way16"
                [ testCase "first"
                    (assertEqual "first input" (bus16 0)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "0") (bit "0") (bit "0")
                        )
                    )
                , testCase "second"
                    (assertEqual "second input" (bus16 1)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "0") (bit "0") (bit "1")
                        )
                    )
                , testCase "third"
                    (assertEqual "third input" (bus16 2)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "0") (bit "1") (bit "0")
                        )
                    )
                , testCase "fourth"
                    (assertEqual "fourth input" (bus16 3)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "0") (bit "1") (bit "1")
                        )
                    )
                , testCase "fifth"
                    (assertEqual "fifth input" (bus16 4)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "1") (bit "0") (bit "0")
                        )
                    )
                , testCase "sixth"
                    (assertEqual "sixth input" (bus16 5)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "1") (bit "0") (bit "1")
                        )
                    )
                , testCase "seventh"
                    (assertEqual "seventh input" (bus16 6)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "1") (bit "1") (bit "0")
                        )
                    )
                , testCase "eigth"
                    (assertEqual "eigth input" (bus16 7)
                        (mux8Way16
                            (bus16 0)
                            (bus16 1)
                            (bus16 2)
                            (bus16 3)
                            (bus16 4)
                            (bus16 5)
                            (bus16 6)
                            (bus16 7)
                            (bit "1") (bit "1") (bit "1")
                        )
                    )
                ]
            ]
        , testGroup "Demultiplexors"
            [ testGroup "dmux4Way"
                [ testCase "first off" (assertEqual "0" (bit "0", bit "0", bit "0", bit "0") (dmux4Way (bit "0") (bit "0") (bit "0")))
                , testCase "first on" (assertEqual "1" (bit "1", bit "0", bit "0", bit "0") (dmux4Way (bit "0") (bit "0") (bit "1")))
                , testCase "second off" (assertEqual "0" (bit "0", bit "0", bit "0", bit "0") (dmux4Way (bit "0") (bit "1") (bit "0")))
                , testCase "second on" (assertEqual "1" (bit "0", bit "1", bit "0", bit "0") (dmux4Way (bit "0") (bit "1") (bit "1")))
                , testCase "third off" (assertEqual "0" (bit "0", bit "0", bit "0", bit "0") (dmux4Way (bit "0") (bit "1") (bit "0")))
                , testCase "third on" (assertEqual "1" (bit "0", bit "0", bit "1", bit "0") (dmux4Way (bit "1") (bit "0") (bit "1")))
                , testCase "fourth off" (assertEqual "0" (bit "0", bit "0", bit "0", bit "0") (dmux4Way (bit "0") (bit "1") (bit "0")))
                , testCase "fourth on" (assertEqual "1" (bit "0", bit "0", bit "0", bit "1") (dmux4Way (bit "1") (bit "1") (bit "1")))
                ]
            , testGroup "dmux8Way"
                [ testCase "first off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "0") (bit "0") (bit "0"))
                    )
                , testCase "first on"
                    (assertEqual "1"
                        (bit "1", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "0") (bit "0") (bit "1"))
                    )
                , testCase "second off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "0") (bit "1") (bit "0"))
                    )
                , testCase "second on"
                    (assertEqual "1"
                        (bit "0", bit "1", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "0") (bit "1") (bit "1"))
                    )
                , testCase "third off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "0") (bit "1") (bit "0"))
                    )
                , testCase "third on"
                    (assertEqual "1"
                        (bit "0", bit "0", bit "1", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "1") (bit "0") (bit "1"))
                    )
                , testCase "fourth off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "0") (bit "1") (bit "0"))
                    )
                , testCase "fourth on"
                    (assertEqual "1"
                        (bit "0", bit "0", bit "0", bit "1", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "0") (bit "1") (bit "1") (bit "1"))
                    )
                , testCase "fifth off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "1") (bit "0") (bit "0") (bit "0"))
                    )
                , testCase "fifth on"
                    (assertEqual "1"
                        (bit "0", bit "0", bit "0", bit "0", bit "1", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "1") (bit "0") (bit "0") (bit "1"))
                    )
                , testCase "sixth off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "1") (bit "0") (bit "1") (bit "0"))
                    )
                , testCase "sixth on"
                    (assertEqual "1"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "1", bit "0", bit "0")
                        (dmux8Way (bit "1") (bit "0") (bit "1") (bit "1"))
                    )
                , testCase "seventh off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "1") (bit "0") (bit "1") (bit "0"))
                    )
                , testCase "seventh on"
                    (assertEqual "1"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "1", bit "0")
                        (dmux8Way (bit "1") (bit "1") (bit "0") (bit "1"))
                    )
                , testCase "eigth off"
                    (assertEqual "0"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0")
                        (dmux8Way (bit "1") (bit "0") (bit "1") (bit "0"))
                    )
                , testCase "eigth on"
                    (assertEqual "1"
                        (bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "0", bit "1")
                        (dmux8Way (bit "1") (bit "1") (bit "1") (bit "1"))
                    )
                ]
            ]
        ]
