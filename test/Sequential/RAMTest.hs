module Sequential.RAMTest
    ( test_ram
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Util
import Sequential.DFF (runState)
import Bus.Data (bus16Zero)
import Sequential.RAM

empty :: Memory8
empty = Memory8 bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero

two :: Memory8
two = Memory8 bus16Zero (bus16 2) bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero

-- tests
test_ram :: TestTree
test_ram =
    testGroup
        "Sequential.RAM"
        [ testGroup "ram8"
            [ testCase "" (assertEqual "" (bus16 0, empty) (runState (ram8 (bus16 0) (bit "0", bit "0", bit "0") (bit "0")) empty))
            , testCase "" (assertEqual "" (bus16 0, Memory8 bus16Zero (bus16 2) bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero) (runState (ram8 (bus16 2) (bit "0", bit "0", bit "1") (bit "1")) empty))
            , testCase "" (assertEqual "" (bus16 2, two) (runState (ram8 (bus16 484) (bit "0", bit "0", bit "1") (bit "0")) two))
            , testCase "" (assertEqual "" (bus16 0, Memory8 bus16Zero bus16Zero (bus16 2) bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero) (runState (ram8 (bus16 2) (bit "0", bit "1", bit "0") (bit "1")) empty))
            , testCase "" (assertEqual "" (bus16 0, Memory8 bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero bus16Zero (bus16 30843)) (runState (ram8 (bus16 30843) (bit "1", bit "1", bit "1") (bit "1")) empty))
            ]
        ]
