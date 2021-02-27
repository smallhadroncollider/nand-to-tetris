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
import Sequential.DFF (state)
import Sequential.Registers

repeatState :: (Bus16Output, Bus16Output) -> (Bus16Input, Binary) -> (Bus16Output, Bus16Output)
repeatState (_, init) (v, l) = state (register v l) init

runSequence :: Bus16Input -> [Bus16Input] -> [Binary] -> (Bus16Output, Bus16Output)
runSequence initial values load = foldl' repeatState (bus16 0, initial) zipped
    where zipped = zip values load

-- tests
test_registers :: TestTree
test_registers =
    testGroup
        "Sequential.Registers"
        [ testGroup "registerBit"
            [ testCase "" (assertEqual "" (_0, _0) (state (registerBit _0 _0) _0))
            , testCase "" (assertEqual "" (_0, _0) (state (registerBit _0 _1) _0))
            , testCase "" (assertEqual "" (_0, _0) (state (registerBit _1 _0) _0))
            , testCase "" (assertEqual "" (_0, _1) (state (registerBit _1 _1) _0))
            , testCase "" (assertEqual "" (_1, _1) (state (registerBit _0 _0) _1))
            , testCase "" (assertEqual "" (_1, _0) (state (registerBit _0 _1) _1))
            , testCase "" (assertEqual "" (_1, _1) (state (registerBit _1 _0) _1))
            , testCase "" (assertEqual "" (_1, _1) (state (registerBit _1 _1) _1))
            ]
        , testGroup "register"
            [ testCase "" (assertEqual "" (bus16 0, bus16 0) (state (register (bus16 348) _0) (bus16 0)))
            , testCase "" (assertEqual "" (bus16 0, bus16 348) (state (register (bus16 348) _1) (bus16 0)))
            ]
        , testGroup "register sequences"
            [ testCase "" (assertEqual "" (bus16 0, bus16 0) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [_0, _0, _0, _0]))
            , testCase "" (assertEqual "" (bus16 2, bus16 2) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [_0, _1, _0, _0]))
            , testCase "" (assertEqual "" (bus16 3, bus16 3) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [_0, _0, _1, _0]))
            , testCase "" (assertEqual "" (bus16 0, bus16 4) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [_0, _0, _0, _1]))
            , testCase "" (assertEqual "" (bus16 2, bus16 4) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4] [_0, _1, _0, _1]))
            , testCase "" (assertEqual "" (bus16 4, bus16 6) (runSequence (bus16 0) [bus16 1, bus16 2, bus16 3, bus16 4, bus16 5, bus16 6] [_0, _1, _0, _1, _0, _1]))
            ]
        ]
