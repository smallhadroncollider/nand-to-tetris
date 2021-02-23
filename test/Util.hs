{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}
module Util where

import Prelude (Show, Eq, Bool (True, False), show, (==), (<$>), (&&), concat)

import Data.List ((!!))
import Data.Text (Text, chunksOf)

import Bit.Data (Binary (One, Zero))
import Bus.Data (Bus16 (Bus16))


-- binary
instance Eq Binary where
    One == One = True
    Zero == Zero = True
    _ == _ = False

instance Show Binary where
    show Zero = "0"
    show One  = "1"

bit :: Text -> Binary
bit "0" = Zero
bit "1" = One


-- bus
instance Eq Bus16 where
    (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) == (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = (a0 == b0) && (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4) && (a5 == b5) && (a6 == b6) && (a7 == b7) && (a8 == b8) && (a9 == b9) && (a10 == b10) && (a11 == b11) && (a12 == b12) && (a13 == b13) && (a14 == b14) && (a15 == b15)

instance Show Bus16 where
    show (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) = concat (show <$> [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15])

bus16 :: Text -> Bus16
bus16 values = Bus16 (ls !! 0) (ls !! 1) (ls !! 2) (ls !! 3) (ls !! 4) (ls !! 5) (ls !! 6) (ls !! 7) (ls !! 8) (ls !! 9) (ls !! 10) (ls !! 11) (ls !! 12) (ls !! 13) (ls !! 14) (ls !! 15)
    where ls = bit <$> chunksOf 1 values
