module Sequential.ProgramCounterTest
    ( test_pc
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Sequential.DFF (state)
import Sequential.ProgramCounter

-- tests
test_pc :: TestTree
test_pc =
    testGroup
        "Sequential.Registers"
        [ testGroup "pc"
            [ testCase "reset" (assertEqual "" (bus16 34, bus16 0) (state (pc (bus16 2) _0 _0 _1) (bus16 34)))
            , testCase "reset" (assertEqual "" (bus16 34, bus16 0) (state (pc (bus16 2) _0 _1 _1) (bus16 34)))
            , testCase "reset" (assertEqual "" (bus16 34, bus16 0) (state (pc (bus16 2) _1 _0 _1) (bus16 34)))
            , testCase "reset" (assertEqual "" (bus16 34, bus16 0) (state (pc (bus16 2) _1 _1 _1) (bus16 34)))
            , testCase "load" (assertEqual "" (bus16 0, bus16 98) (state (pc (bus16 98) _0 _1 _0) (bus16 0)))
            , testCase "load" (assertEqual "" (bus16 0, bus16 98) (state (pc (bus16 98) _1 _1 _0) (bus16 0)))
            , testCase "incremement" (assertEqual "" (bus16 2, bus16 3) (state (pc (bus16 389) _1 _0 _0) (bus16 2)))
            , testCase "read" (assertEqual "" (bus16 39, bus16 39) (state (pc (bus16 7) _0 _0 _0) (bus16 39)))
            ]
        ]
