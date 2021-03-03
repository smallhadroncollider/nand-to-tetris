{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}
module Util
    ( bus8
    , bus16
    , toBinaryText
    , _0
    , _1
    ) where

import Prelude (Eq, Show, Int, Bool (True, False), (==), ($), (<), (^), (-), (&&), (>=), (+), (<$>), (!!), concat, show)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (intercalate)

import Data.Text (Text, chunksOf, pack, justifyRight)

import Bit.Data (Binary (One, Zero))
import Bus.Data (Bus16 (Bus16), Bus8 (Bus8))
import Sequential.RAM


-- binary
instance Eq Binary where
    One == One = True
    Zero == Zero = True
    _ == _ = False

instance Show Binary where
    show Zero = "0"
    show One  = "1"

_0 :: Binary
_0 = Zero

_1 :: Binary
_1 = One

bit :: Text -> Binary
bit "0" = Zero
bit "1" = One


-- buses
toBinary :: Int -> Text
toBinary n = pack $ showIntAtBase 2 intToDigit n ""

toBinaryText :: Int -> Int -> Text
toBinaryText n len
    | n < (2 ^ (len - 1)) && n >= 0 = justifyRight len '0' (toBinary n)
    | n >= (-(2 ^ (len - 1))) && n < 0 = toBinary ((2 ^ len) + n)


-- bus8
instance Eq Bus8 where
    (Bus8 a0 a1 a2 a3 a4 a5 a6 a7) == (Bus8 b0 b1 b2 b3 b4 b5 b6 b7) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7)

instance Show Bus8 where
    show (Bus8 i0 i1 i2 i3 i4 i5 i6 i7) = concat (show <$> [i0, i1, i2, i3, i4, i5, i6, i7])

bus8FromText :: Text -> Bus8
bus8FromText values = Bus8 (ls !! 0) (ls !! 1) (ls !! 2) (ls !! 3) (ls !! 4) (ls !! 5) (ls !! 6) (ls !! 7)
    where ls = bit <$> chunksOf 1 values

bus8 :: Int -> Bus8
bus8 n = bus8FromText $ toBinaryText n 8


-- bus16
instance Eq Bus16 where
    (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) == (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7) && (a8 == b8) && (a9 == b9) && (a10 == b10) && (a11 == b11) && (a12 == b12) && (a13 == b13) && (a14 == b14) && (a15 == b15)

instance Show Bus16 where
    show (Bus16 i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15) = concat (show <$> [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15])

bus16FromText :: Text -> Bus16
bus16FromText values = Bus16 (ls !! 0) (ls !! 1) (ls !! 2) (ls !! 3) (ls !! 4) (ls !! 5) (ls !! 6) (ls !! 7) (ls !! 8) (ls !! 9) (ls !! 10) (ls !! 11) (ls !! 12) (ls !! 13) (ls !! 14) (ls !! 15)
    where ls = bit <$> chunksOf 1 values

bus16 :: Int -> Bus16
bus16 n = bus16FromText $ toBinaryText n 16


-- memory 8
instance Eq Memory8 where
    (Memory8 a0 a1 a2 a3 a4 a5 a6 a7) == (Memory8 b0 b1 b2 b3 b4 b5 b6 b7) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7)

instance Show Memory8 where
    show (Memory8 i0 i1 i2 i3 i4 i5 i6 i7) = intercalate " / " (show <$> [i0, i1, i2, i3, i4, i5, i6, i7])


-- memory 64
instance Eq Memory64 where
    (Memory64 a0 a1 a2 a3 a4 a5 a6 a7) == (Memory64 b0 b1 b2 b3 b4 b5 b6 b7) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7)

instance Show Memory64 where
    show (Memory64 i0 i1 i2 i3 i4 i5 i6 i7) = intercalate " / " (show <$> [i0, i1, i2, i3, i4, i5, i6, i7])


-- memory 512
instance Eq Memory512 where
    (Memory512 a0 a1 a2 a3 a4 a5 a6 a7) == (Memory512 b0 b1 b2 b3 b4 b5 b6 b7) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7)

instance Show Memory512 where
    show (Memory512 i0 i1 i2 i3 i4 i5 i6 i7) = intercalate " / " (show <$> [i0, i1, i2, i3, i4, i5, i6, i7])


-- memory 4k
instance Eq Memory4K where
    (Memory4K a0 a1 a2 a3 a4 a5 a6 a7) == (Memory4K b0 b1 b2 b3 b4 b5 b6 b7) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7)

instance Show Memory4K where
    show (Memory4K i0 i1 i2 i3 i4 i5 i6 i7) = intercalate " / " (show <$> [i0, i1, i2, i3, i4, i5, i6, i7])


-- memory 16k
instance Eq Memory16K where
    (Memory16K a0 a1 a2 a3) == (Memory16K b0 b1 b2 b3) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3)

instance Show Memory16K where
    show (Memory16K i0 i1 i2 i3) = intercalate " / " (show <$> [i0, i1, i2, i3])
