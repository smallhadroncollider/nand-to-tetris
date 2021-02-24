module ALU.ALUTest
    ( test_alu
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import ALU.ALU

-- tests
test_alu :: TestTree
test_alu =
    testGroup
        "ALU.ALU"
        [ testGroup "aluBasic"
            [ testCase "0"
                (assertEqual "0"
                    (bus16 0)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "0") (bit "1") (bit "0") (bit "1") (bit "0")
                    --  Also 0 with
                    --  (bit "1") (bit "0") (bit "1") (bit "1") (bit "0") (bit "0")
                    --  (bit "1") (bit "1") (bit "1") (bit "0") (bit "0") (bit "0")
                    )
                )
            , testCase "1"
                (assertEqual "1"
                    (bus16 1)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "1") (bit "1") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "-1"
                (assertEqual "-1"
                    (bus16 (-1))
                    (aluBasic
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "1") (bit "1") (bit "0") (bit "1") (bit "0")
                    --  Also -1 with
                    --  (bit "1") (bit "1") (bit "1") (bit "1") (bit "0") (bit "0")
                    --  (bit "1") (bit "0") (bit "1") (bit "0") (bit "0") (bit "1")
                    --  (bit "1") (bit "0") (bit "1") (bit "1") (bit "1") (bit "0")
                    )
                )
            , testCase "x"
                (assertEqual "x"
                    (bus16 12)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "0") (bit "0")
                    )
                )
            , testCase "y"
                (assertEqual "y"
                    (bus16 34)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "0") (bit "0")
                    )
                )
            , testCase "!x"
                (assertEqual "!x"
                    (bus16 (-2))
                    (aluBasic
                        (bus16 1) (bus16 34)
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "0") (bit "1")
                    )
                )
            , testCase "!y"
                (assertEqual "!y"
                    (bus16 2)
                    (aluBasic
                        (bus16 334) (bus16 (-3))
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "0") (bit "1")
                    )
                )
            , testCase "-x"
                (assertEqual "-x"
                    (bus16 (-3358))
                    (aluBasic
                        (bus16 3358) (bus16 3434)
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "-y"
                (assertEqual "-y"
                    (bus16 8392)
                    (aluBasic
                        (bus16 3358) (bus16 (-8392))
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "1") (bit "1")
                    )
                )
            , testCase "x+1"
                (assertEqual "x+1"
                    (bus16 3359)
                    (aluBasic
                        (bus16 3358) (bus16 (-8392))
                        (bit "0") (bit "1") (bit "1") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "y+1"
                (assertEqual "y+1"
                    (bus16 (-8291))
                    (aluBasic
                        (bus16 3358) (bus16 (-8292))
                        (bit "1") (bit "1") (bit "0") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "x-1"
                (assertEqual "x-1"
                    (bus16 3357)
                    (aluBasic
                        (bus16 3358) (bus16 (-8292))
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "1") (bit "0")
                    )
                )
            , testCase "y-1"
                (assertEqual "y-1"
                    (bus16 (-8293))
                    (aluBasic
                        (bus16 3358) (bus16 (-8292))
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "1") (bit "0")
                    )
                )
            , testCase "x+y"
                (assertEqual "x+y"
                    (bus16 4047)
                    (aluBasic
                        (bus16 8) (bus16 4039)
                        (bit "0") (bit "0") (bit "0") (bit "0") (bit "1") (bit "0")
                    )
                )
            , testCase "x-y"
                (assertEqual "x-y"
                    (bus16 (-349))
                    (aluBasic
                        (bus16 35) (bus16 384)
                        (bit "0") (bit "1") (bit "0") (bit "0") (bit "1") (bit "1")
                    )
                )
            , testCase "y-x"
                (assertEqual "y-x"
                    (bus16 (-4338))
                    (aluBasic
                        (bus16 3489) (bus16 (-849))
                        (bit "0") (bit "0") (bit "0") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "x&y"
                (assertEqual "x&y"
                    (bus16 4)
                    (aluBasic
                        (bus16 12) (bus16 4)
                        (bit "0") (bit "0") (bit "0") (bit "0") (bit "0") (bit "0")
                    )
                )
            , testCase "x|y"
                (assertEqual "x|y"
                    (bus16 13)
                    (aluBasic
                        (bus16 12) (bus16 1)
                        (bit "0") (bit "1") (bit "0") (bit "1") (bit "0") (bit "1")
                    )
                )
            ]
        , testGroup "alu"
            [ testCase "0"
                (assertEqual "0"
                    (bus16 0, bit "1", bit "0")
                    (alu
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "0") (bit "1") (bit "0") (bit "1") (bit "0")
                    )
                )
            , testCase "1"
                (assertEqual "1"
                    (bus16 1, bit "0", bit "0")
                    (alu
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "1") (bit "1") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "-1"
                (assertEqual "-1"
                    (bus16 (-1), bit "0", bit "1")
                    (alu
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "1") (bit "1") (bit "0") (bit "1") (bit "0")
                    )
                )
            , testCase "x"
                (assertEqual "x"
                    (bus16 12, bit "0", bit "0")
                    (alu
                        (bus16 12) (bus16 34)
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "0") (bit "0")
                    )
                )
            , testCase "y"
                (assertEqual "y"
                    (bus16 34, bit "0", bit "0")
                    (alu
                        (bus16 12) (bus16 34)
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "0") (bit "0")
                    )
                )
            , testCase "!x"
                (assertEqual "!x"
                    (bus16 (-2), bit "0", bit "1")
                    (alu
                        (bus16 1) (bus16 34)
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "0") (bit "1")
                    )
                )
            , testCase "!y"
                (assertEqual "!y"
                    (bus16 2, bit "0", bit "0")
                    (alu
                        (bus16 334) (bus16 (-3))
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "0") (bit "1")
                    )
                )
            , testCase "-x"
                (assertEqual "-x"
                    (bus16 (-3358), bit "0", bit "1")
                    (alu
                        (bus16 3358) (bus16 3434)
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "-y"
                (assertEqual "-y"
                    (bus16 8392, bit "0", bit "0")
                    (alu
                        (bus16 3358) (bus16 (-8392))
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "1") (bit "1")
                    )
                )
            , testCase "x+1"
                (assertEqual "x+1"
                    (bus16 3359, bit "0", bit "0")
                    (alu
                        (bus16 3358) (bus16 (-8392))
                        (bit "0") (bit "1") (bit "1") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "y+1"
                (assertEqual "y+1"
                    (bus16 (-8291), bit "0", bit "1")
                    (alu
                        (bus16 3358) (bus16 (-8292))
                        (bit "1") (bit "1") (bit "0") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "x-1"
                (assertEqual "x-1"
                    (bus16 3357, bit "0", bit "0")
                    (alu
                        (bus16 3358) (bus16 (-8292))
                        (bit "0") (bit "0") (bit "1") (bit "1") (bit "1") (bit "0")
                    )
                )
            , testCase "y-1"
                (assertEqual "y-1"
                    (bus16 (-8293), bit "0", bit "1")
                    (alu
                        (bus16 3358) (bus16 (-8292))
                        (bit "1") (bit "1") (bit "0") (bit "0") (bit "1") (bit "0")
                    )
                )
            , testCase "x+y"
                (assertEqual "x+y"
                    (bus16 4047, bit "0", bit "0")
                    (alu
                        (bus16 8) (bus16 4039)
                        (bit "0") (bit "0") (bit "0") (bit "0") (bit "1") (bit "0")
                    )
                )
            , testCase "x-y"
                (assertEqual "x-y"
                    (bus16 (-349), bit "0", bit "1")
                    (alu
                        (bus16 35) (bus16 384)
                        (bit "0") (bit "1") (bit "0") (bit "0") (bit "1") (bit "1")
                    )
                )
            , testCase "y-x"
                (assertEqual "y-x"
                    (bus16 (-4338), bit "0", bit "1")
                    (alu
                        (bus16 3489) (bus16 (-849))
                        (bit "0") (bit "0") (bit "0") (bit "1") (bit "1") (bit "1")
                    )
                )
            , testCase "x&y"
                (assertEqual "x&y"
                    (bus16 4, bit "0", bit "0")
                    (alu
                        (bus16 12) (bus16 4)
                        (bit "0") (bit "0") (bit "0") (bit "0") (bit "0") (bit "0")
                    )
                )
            , testCase "x|y"
                (assertEqual "x|y"
                    (bus16 13, bit "0", bit "0")
                    (alu
                        (bus16 12) (bus16 1)
                        (bit "0") (bit "1") (bit "0") (bit "1") (bit "0") (bit "1")
                    )
                )
            ]
        ]
