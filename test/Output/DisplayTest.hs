module Output.DisplayTest
    ( test_display
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Output.Display

-- tests
test_display :: TestTree
test_display =
    testGroup
        "Output.Display"
        [ testGroup "transform"
            [ testCase "2 x 2 => 1 x 1"
                (assertEqual "join arrays"
                    [
                        [[_0, _1, _1, _0]]
                    ]
                    (transform [
                        [_0, _1],
                        [_1, _0]
                    ])
                )

            , testCase "4 x 4 => 2 x 2"
                (assertEqual "join arrays"
                    [
                        [[_0, _1, _1, _0] , [_1, _1, _0, _0]]
                      , [[_1, _1, _1, _0] , [_1, _0, _1, _1]]
                    ]
                    (transform [
                        [_0, _1, _1, _1]
                      , [_1, _0, _0, _0]
                      , [_1, _1, _1, _0]
                      , [_1, _0, _1, _1]
                    ])
                )
            ]
        , testGroup "keys"
            [ testCase "2 x 2 => 1 x 1"
                (assertEqual "join arrays"
                    [
                        [6]
                    ]
                    (keys [
                        [_0, _1],
                        [_1, _0]
                    ])
                )

            , testCase "4 x 4 => 2 x 2"
                (assertEqual "join arrays"
                    [
                        [6 , 12]
                      , [14 , 11]
                    ]
                    (keys [
                        [_0, _1, _1, _1]
                      , [_1, _0, _0, _0]
                      , [_1, _1, _1, _0]
                      , [_1, _0, _1, _1]
                    ])
                )
            ]
        , testGroup "output"
            [ testCase "2 x 2 => 1 x 1"
                (assertEqual "join arrays"
                    [
                        "▞"
                    ]
                    (output [
                        [_0, _1],
                        [_1, _0]
                    ])
                )

            , testCase "4 x 4 => 2 x 2"
                (assertEqual "join arrays"
                    [
                        "▞▀"
                      , "▛▙"
                    ]
                    (output [
                        [_0, _1, _1, _1]
                      , [_1, _0, _0, _0]
                      , [_1, _1, _1, _0]
                      , [_1, _0, _1, _1]
                    ])
                )

            , testCase "face"
                (assertEqual "join arrays"
                    [ "▗▖▗▖"
                    , "▝▘▝▘"
                    , "▗  ▖"
                    , "▝▀▀▘"
                    ]

                    (output [
                        [_0, _0, _0, _0, _0, _0, _0, _0]
                      , [_0, _1, _1, _0, _0, _1, _1, _0]
                      , [_0, _1, _1, _0, _0, _1, _1, _0]
                      , [_0, _0, _0, _0, _0, _0, _0, _0]
                      , [_0, _0, _0, _0, _0, _0, _0, _0]
                      , [_0, _1, _0, _0, _0, _0, _1, _0]
                      , [_0, _1, _1, _1, _1, _1, _1, _0]
                      , [_0, _0, _0, _0, _0, _0, _0, _0]
                    ])
                )
            ]
        ]
