module Sequential.RegistersTest
    ( test_registers
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.State.Strict (runState)

import Util
import Sequential.Registers

-- tests
test_registers :: TestTree
test_registers =
    testGroup
        "Sequential.Registers"
        [ testGroup "registerBit"
            [ testCase "" (assertEqual "" (bit "0", bit "0") (runState (registerBit (bit "0") (bit "0")) (bit "0")))
            , testCase "" (assertEqual "" (bit "0", bit "0") (runState (registerBit (bit "0") (bit "1")) (bit "0")))
            , testCase "" (assertEqual "" (bit "0", bit "0") (runState (registerBit (bit "1") (bit "0")) (bit "0")))
            , testCase "" (assertEqual "" (bit "0", bit "1") (runState (registerBit (bit "1") (bit "1")) (bit "0")))
            , testCase "" (assertEqual "" (bit "1", bit "1") (runState (registerBit (bit "0") (bit "0")) (bit "1")))
            , testCase "" (assertEqual "" (bit "1", bit "0") (runState (registerBit (bit "0") (bit "1")) (bit "1")))
            , testCase "" (assertEqual "" (bit "1", bit "1") (runState (registerBit (bit "1") (bit "0")) (bit "1")))
            , testCase "" (assertEqual "" (bit "1", bit "1") (runState (registerBit (bit "1") (bit "1")) (bit "1")))
            ]

        , testGroup "register"
            [ testCase "" (assertEqual "" (bus16 0, bus16 0) (runState (register (bus16 348) (bit "0")) (bus16 0)))
            , testCase "" (assertEqual "" (bus16 0, bus16 348) (runState (register (bus16 348) (bit "1")) (bus16 0)))
            ]
        ]
