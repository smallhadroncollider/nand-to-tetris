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
                        _1 _0 _1 _0 _1 _0
                    --  Also 0 with
                    --  _1 _0 _1 _1 _0 _0
                    --  _1 _1 _1 _0 _0 _0
                    )
                )
            , testCase "1"
                (assertEqual "1"
                    (bus16 1)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        _1 _1 _1 _1 _1 _1
                    )
                )
            , testCase "-1"
                (assertEqual "-1"
                    (bus16 (-1))
                    (aluBasic
                        (bus16 12) (bus16 34)
                        _1 _1 _1 _0 _1 _0
                    --  Also -1 with
                    --  _1 _1 _1 _1 _0 _0
                    --  _1 _0 _1 _0 _0 _1
                    --  _1 _0 _1 _1 _1 _0
                    )
                )
            , testCase "x"
                (assertEqual "x"
                    (bus16 12)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        _0 _0 _1 _1 _0 _0
                    )
                )
            , testCase "y"
                (assertEqual "y"
                    (bus16 34)
                    (aluBasic
                        (bus16 12) (bus16 34)
                        _1 _1 _0 _0 _0 _0
                    )
                )
            , testCase "!x"
                (assertEqual "!x"
                    (bus16 (-2))
                    (aluBasic
                        (bus16 1) (bus16 34)
                        _0 _0 _1 _1 _0 _1
                    )
                )
            , testCase "!y"
                (assertEqual "!y"
                    (bus16 2)
                    (aluBasic
                        (bus16 334) (bus16 (-3))
                        _1 _1 _0 _0 _0 _1
                    )
                )
            , testCase "-x"
                (assertEqual "-x"
                    (bus16 (-3358))
                    (aluBasic
                        (bus16 3358) (bus16 3434)
                        _0 _0 _1 _1 _1 _1
                    )
                )
            , testCase "-y"
                (assertEqual "-y"
                    (bus16 8392)
                    (aluBasic
                        (bus16 3358) (bus16 (-8392))
                        _1 _1 _0 _0 _1 _1
                    )
                )
            , testCase "x+1"
                (assertEqual "x+1"
                    (bus16 3359)
                    (aluBasic
                        (bus16 3358) (bus16 (-8392))
                        _0 _1 _1 _1 _1 _1
                    )
                )
            , testCase "y+1"
                (assertEqual "y+1"
                    (bus16 (-8291))
                    (aluBasic
                        (bus16 3358) (bus16 (-8292))
                        _1 _1 _0 _1 _1 _1
                    )
                )
            , testCase "x-1"
                (assertEqual "x-1"
                    (bus16 3357)
                    (aluBasic
                        (bus16 3358) (bus16 (-8292))
                        _0 _0 _1 _1 _1 _0
                    )
                )
            , testCase "y-1"
                (assertEqual "y-1"
                    (bus16 (-8293))
                    (aluBasic
                        (bus16 3358) (bus16 (-8292))
                        _1 _1 _0 _0 _1 _0
                    )
                )
            , testCase "x+y"
                (assertEqual "x+y"
                    (bus16 4047)
                    (aluBasic
                        (bus16 8) (bus16 4039)
                        _0 _0 _0 _0 _1 _0
                    )
                )
            , testCase "x-y"
                (assertEqual "x-y"
                    (bus16 (-349))
                    (aluBasic
                        (bus16 35) (bus16 384)
                        _0 _1 _0 _0 _1 _1
                    )
                )
            , testCase "y-x"
                (assertEqual "y-x"
                    (bus16 (-4338))
                    (aluBasic
                        (bus16 3489) (bus16 (-849))
                        _0 _0 _0 _1 _1 _1
                    )
                )
            , testCase "x&y"
                (assertEqual "x&y"
                    (bus16 4)
                    (aluBasic
                        (bus16 12) (bus16 4)
                        _0 _0 _0 _0 _0 _0
                    )
                )
            , testCase "x|y"
                (assertEqual "x|y"
                    (bus16 13)
                    (aluBasic
                        (bus16 12) (bus16 1)
                        _0 _1 _0 _1 _0 _1
                    )
                )
            ]
        , testGroup "alu"
            [ testCase "0"
                (assertEqual "0"
                    (bus16 0, _1, _0)
                    (alu
                        (bus16 12) (bus16 34)
                        _1 _0 _1 _0 _1 _0
                    )
                )
            , testCase "1"
                (assertEqual "1"
                    (bus16 1, _0, _0)
                    (alu
                        (bus16 12) (bus16 34)
                        _1 _1 _1 _1 _1 _1
                    )
                )
            , testCase "-1"
                (assertEqual "-1"
                    (bus16 (-1), _0, _1)
                    (alu
                        (bus16 12) (bus16 34)
                        _1 _1 _1 _0 _1 _0
                    )
                )
            , testCase "x"
                (assertEqual "x"
                    (bus16 12, _0, _0)
                    (alu
                        (bus16 12) (bus16 34)
                        _0 _0 _1 _1 _0 _0
                    )
                )
            , testCase "y"
                (assertEqual "y"
                    (bus16 34, _0, _0)
                    (alu
                        (bus16 12) (bus16 34)
                        _1 _1 _0 _0 _0 _0
                    )
                )
            , testCase "!x"
                (assertEqual "!x"
                    (bus16 (-2), _0, _1)
                    (alu
                        (bus16 1) (bus16 34)
                        _0 _0 _1 _1 _0 _1
                    )
                )
            , testCase "!y"
                (assertEqual "!y"
                    (bus16 2, _0, _0)
                    (alu
                        (bus16 334) (bus16 (-3))
                        _1 _1 _0 _0 _0 _1
                    )
                )
            , testCase "-x"
                (assertEqual "-x"
                    (bus16 (-3358), _0, _1)
                    (alu
                        (bus16 3358) (bus16 3434)
                        _0 _0 _1 _1 _1 _1
                    )
                )
            , testCase "-y"
                (assertEqual "-y"
                    (bus16 8392, _0, _0)
                    (alu
                        (bus16 3358) (bus16 (-8392))
                        _1 _1 _0 _0 _1 _1
                    )
                )
            , testCase "x+1"
                (assertEqual "x+1"
                    (bus16 3359, _0, _0)
                    (alu
                        (bus16 3358) (bus16 (-8392))
                        _0 _1 _1 _1 _1 _1
                    )
                )
            , testCase "y+1"
                (assertEqual "y+1"
                    (bus16 (-8291), _0, _1)
                    (alu
                        (bus16 3358) (bus16 (-8292))
                        _1 _1 _0 _1 _1 _1
                    )
                )
            , testCase "x-1"
                (assertEqual "x-1"
                    (bus16 3357, _0, _0)
                    (alu
                        (bus16 3358) (bus16 (-8292))
                        _0 _0 _1 _1 _1 _0
                    )
                )
            , testCase "y-1"
                (assertEqual "y-1"
                    (bus16 (-8293), _0, _1)
                    (alu
                        (bus16 3358) (bus16 (-8292))
                        _1 _1 _0 _0 _1 _0
                    )
                )
            , testCase "x+y"
                (assertEqual "x+y"
                    (bus16 4047, _0, _0)
                    (alu
                        (bus16 8) (bus16 4039)
                        _0 _0 _0 _0 _1 _0
                    )
                )
            , testCase "x-y"
                (assertEqual "x-y"
                    (bus16 (-349), _0, _1)
                    (alu
                        (bus16 35) (bus16 384)
                        _0 _1 _0 _0 _1 _1
                    )
                )
            , testCase "y-x"
                (assertEqual "y-x"
                    (bus16 (-4338), _0, _1)
                    (alu
                        (bus16 3489) (bus16 (-849))
                        _0 _0 _0 _1 _1 _1
                    )
                )
            , testCase "x&y"
                (assertEqual "x&y"
                    (bus16 4, _0, _0)
                    (alu
                        (bus16 12) (bus16 4)
                        _0 _0 _0 _0 _0 _0
                    )
                )
            , testCase "x|y"
                (assertEqual "x|y"
                    (bus16 13, _0, _0)
                    (alu
                        (bus16 12) (bus16 1)
                        _0 _1 _0 _1 _0 _1
                    )
                )
            ]
        ]
