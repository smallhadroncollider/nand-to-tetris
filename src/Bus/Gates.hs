module Bus.Gates
    ( not16
    , and16
    , or16
    , mux16
    , or8Way
    ) where

import Bus.Data (Bus16 (Bus16), Bus8 (Bus8))
import Bit.Gates

-- bus16 gates
type Bus16Input = Bus16
type Bus16Output = Bus16

not16 :: Bus16Input -> Bus16Output
not16
    (Bus16 i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15)
    =
    Bus16
        (not i0)
        (not i1)
        (not i2)
        (not i3)
        (not i4)
        (not i5)
        (not i6)
        (not i7)
        (not i8)
        (not i9)
        (not i10)
        (not i11)
        (not i12)
        (not i13)
        (not i14)
        (not i15)

and16 :: Bus16Input -> Bus16Input -> Bus16Output
and16
    (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
    =
    Bus16
        (a0 `and` b0)
        (a1 `and` b1)
        (a2 `and` b2)
        (a3 `and` b3)
        (a4 `and` b4)
        (a5 `and` b5)
        (a6 `and` b6)
        (a7 `and` b7)
        (a8 `and` b8)
        (a9 `and` b9)
        (a10 `and` b10)
        (a11 `and` b11)
        (a12 `and` b12)
        (a13 `and` b13)
        (a14 `and` b14)
        (a15 `and` b15)

or16 :: Bus16Input -> Bus16Input -> Bus16Output
or16
    (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
    =
    Bus16
        (a0 `or` b0)
        (a1 `or` b1)
        (a2 `or` b2)
        (a3 `or` b3)
        (a4 `or` b4)
        (a5 `or` b5)
        (a6 `or` b6)
        (a7 `or` b7)
        (a8 `or` b8)
        (a9 `or` b9)
        (a10 `or` b10)
        (a11 `or` b11)
        (a12 `or` b12)
        (a13 `or` b13)
        (a14 `or` b14)
        (a15 `or` b15)

mux16 :: Bus16Input -> Bus16Input -> Selector -> Bus16Output
mux16
    (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
    sel
    =
    Bus16
        (mux a0 b0 sel)
        (mux a1 b1 sel)
        (mux a2 b2 sel)
        (mux a3 b3 sel)
        (mux a4 b4 sel)
        (mux a5 b5 sel)
        (mux a6 b6 sel)
        (mux a7 b7 sel)
        (mux a8 b8 sel)
        (mux a9 b9 sel)
        (mux a10 b10 sel)
        (mux a11 b11 sel)
        (mux a12 b12 sel)
        (mux a13 b13 sel)
        (mux a14 b14 sel)
        (mux a15 b15 sel)


-- bus8 gates
type Bus8Input = Bus8

or8Way :: Bus8Input -> Output
or8Way (Bus8 i0 i1 i2 i3 i4 i5 i6 i7) = i0 `or` i1 `or` i2 `or` i3 `or` i4 `or` i5 `or` i6 `or` i7
