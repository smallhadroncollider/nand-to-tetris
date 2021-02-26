module Sequential.RegistersTest
    ( test_registers
    ) where

import Prelude (zip)

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (foldl')

import Util
import Bit.Data (Binary)
import Bus.Gates (Bus16Input, Bus16Output)
import Sequential.DFF (runState)
import Sequential.Registers

repeatState :: (Bus16Output, Bus16Output) -> (Bus16Input, Binary) -> (Bus16Output, Bus16Output)
repeatState (_, init) (v, l) = runState (register v l) init

runSequence :: Bus16Input -> [Bus16Input] -> [Binary] -> (Bus16Output, Bus16Output)
runSequence initial values load = foldl' repeatState (bus16 0, initial) zipped
    where zipped = zip values load

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
        , testGroup "register sequences"
            [ testCase "" (assertEqual "" (bus16 0, bus16 0) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [bit "0", bit "0", bit "0", bit "0"]))
            , testCase "" (assertEqual "" (bus16 2, bus16 2) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [bit "0", bit "1", bit "0", bit "0"]))
            , testCase "" (assertEqual "" (bus16 3, bus16 3) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [bit "0", bit "0", bit "1", bit "0"]))
            , testCase "" (assertEqual "" (bus16 0, bus16 4) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [bit "0", bit "0", bit "0", bit "1"]))
            , testCase "" (assertEqual "" (bus16 2, bus16 4) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [bit "0", bit "1", bit "0", bit "1"]))
            , testCase "" (assertEqual "" (bus16 4, bus16 6) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4, bus16 5, bus16 6] [bit "0", bit "1", bit "0", bit "1", bit "0", bit "1"]))
            ]
        ]
