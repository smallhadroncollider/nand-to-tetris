module Sequential.RAMTest
    ( test_ram
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Sequential.DFF (state)
import Bus.Data (bus16Zero)
import Sequential.RAM

empty :: Memory8
empty = Memory8 bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero

two :: Memory8
two = Memory8 bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero (bus16 2) bus16Zero

empty64 :: Memory64
empty64 = Memory64 empty empty empty empty empty empty empty empty

two64 :: Memory64
two64 = Memory64 empty empty empty empty empty empty two empty

empty512 :: Memory512
empty512 = Memory512 empty64 empty64 empty64 empty64 empty64 empty64 empty64 empty64

two512 :: Memory512
two512 = Memory512 empty64 empty64 empty64 empty64 empty64 empty64 two64 empty64

empty4K :: Memory4K
empty4K = Memory4K empty512 empty512 empty512 empty512 empty512 empty512 empty512 empty512

two4K :: Memory4K
two4K = Memory4K empty512 empty512 empty512 empty512 empty512 empty512 two512 empty512

empty16K :: Memory16K
empty16K = Memory16K empty4K empty4K empty4K empty4K

two16K :: Memory16K
two16K = Memory16K empty4K empty4K two4K empty4K

-- tests
test_ram :: TestTree
test_ram =
    testGroup
        "Sequential.RAM"
        [ testGroup "ram8"
            [ testCase "8 empty" (assertEqual "" (bus16 0, empty) (state (ram8 (bus16 0) (_0, _0, _0) _0) empty))
            , testCase "8 Write Two" (assertEqual "" (bus16 0, Memory8 bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero (bus16 2) bus16Zero) (state (ram8 (bus16 2) (_0, _0, _1) _1) empty))
            , testCase "8 Read Two" (assertEqual "" (bus16 2, two) (state (ram8 (bus16 484) (_0, _0, _1) _0) two))
            , testCase "8 Write Two (different address)" (assertEqual "" (bus16 0, Memory8 bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero (bus16 2) bus16Zero bus16Zero) (state (ram8 (bus16 2) (_0, _1, _0) _1) empty))
            , testCase "8 Write 30843" (assertEqual "" (bus16 0, Memory8 (bus16 30843) bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero) (state (ram8 (bus16 30843) (_1, _1, _1) _1) empty))
            ]
        , testGroup "ram64"
            [ testCase "64 empty" (assertEqual "" (bus16 0, empty64) (state (ram64 (bus16 0) ((_0, _0, _0), (_0, _0, _0)) _0) empty64))
            , testCase "64 Write Two" (assertEqual "" (bus16 0, two64) (state (ram64 (bus16 2) ((_0, _0, _1), (_0, _0, _1)) _1) empty64))
            , testCase "64 Read Two" (assertEqual "" (bus16 2, two64) (state (ram64 (bus16 2) ((_0, _0, _1), (_0, _0, _1)) _0) two64))
            ]
        , testGroup "ram16K"
            [ testCase "16K empty" (assertEqual "" (bus16 0, empty16K) (state (ram16K (bus16 0) ((_0, _0), (_0, _0, _0), (_0, _0, _0), (_0, _0, _0), (_0, _0, _0)) _0) empty16K))
            , testCase "16K Write Two" (assertEqual "" (bus16 0, two16K) (state (ram16K (bus16 2) ((_0, _1), (_0, _0, _1), (_0, _0, _1), (_0, _0, _1), (_0, _0, _1)) _1) empty16K))
            , testCase "16K Read Two" (assertEqual "" (bus16 2, two16K) (state (ram16K (bus16 2) ((_0, _1), (_0, _0, _1), (_0, _0, _1), (_0, _0, _1), (_0, _0, _1)) _0) two16K))
            ]
        ]
