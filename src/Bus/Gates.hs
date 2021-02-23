module Bus.Gates where

import Bus.Data (Bus16 (Bus16))
import Bit.Gates

not16 :: Bus16 -> Bus16
not16 (Bus16 a b c d e f g h i j k l m n o p) = Bus16 (not a) (not b) (not c) (not d) (not e) (not f) (not g) (not h) (not i) (not j) (not k) (not l) (not m) (not n) (not o) (not p)

and16 :: Bus16 -> Bus16 -> Bus16
and16 (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = Bus16 (a0 `and` b0) (a1 `and` b1) (a2 `and` b2) (a3 `and` b3) (a4 `and` b4) (a5 `and` b5) (a6 `and` b6) (a7 `and` b7) (a8 `and` b8) (a9 `and` b9) (a10 `and` b10) (a11 `and` b11) (a12 `and` b12) (a13 `and` b13) (a14 `and` b14) (a15 `and` b15)

or16 :: Bus16 -> Bus16 -> Bus16
or16 (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = Bus16 (a0 `or` b0) (a1 `or` b1) (a2 `or` b2) (a3 `or` b3) (a4 `or` b4) (a5 `or` b5) (a6 `or` b6) (a7 `or` b7) (a8 `or` b8) (a9 `or` b9) (a10 `or` b10) (a11 `or` b11) (a12 `or` b12) (a13 `or` b13) (a14 `or` b14) (a15 `or` b15)

mux16 :: Bus16 -> Bus16 -> Selector -> Bus16
mux16 (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) sel = Bus16 (mux a0 b0 sel) (mux a1 b1 sel) (mux a2 b2 sel) (mux a3 b3 sel) (mux a4 b4 sel) (mux a5 b5 sel) (mux a6 b6 sel) (mux a7 b7 sel) (mux a8 b8 sel) (mux a9 b9 sel) (mux a10 b10 sel) (mux a11 b11 sel) (mux a12 b12 sel) (mux a13 b13 sel) (mux a14 b14 sel) (mux a15 b15 sel)
