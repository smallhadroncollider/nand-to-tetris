module ALU.Adders
    ( halfAdder
    , fullAdder
    , add16
    , inc16
    ) where

import Bit.Data (Binary (Zero))
import Bit.Gates (Input, Output, xor, and, or)
import Bus.Data (Bus16 (Bus16), bus16One)
import Bus.Gates (Bus16Input, Bus16Output)

type Sum = Output
type Carry = Output

halfAdder :: Input -> Input -> (Sum, Carry)
halfAdder a b = (a `xor` b, a `and` b)

fullAdder :: Input -> Input -> Input -> (Sum, Carry)
fullAdder a b c = (s2, c1 `or` c2)
    where (s1, c1) = halfAdder b c
          (s2, c2) = halfAdder a s1

add16 :: Bus16Input -> Bus16Input -> Bus16Output
add16
    (Bus16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    (Bus16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)

    =

    Bus16 s15 s14 s13 s12 s11 s10 s9 s8 s7 s6 s5 s4 s3 s2 s1 s0

    where (s0, c0) = fullAdder Zero a15 b15
          (s1, c1) = fullAdder c0 a14 b14
          (s2, c2) = fullAdder c1 a13 b13
          (s3, c3) = fullAdder c2 a12 b12
          (s4, c4) = fullAdder c3 a11 b11
          (s5, c5) = fullAdder c4 a10 b10
          (s6, c6) = fullAdder c5 a9 b9
          (s7, c7) = fullAdder c6 a8 b8
          (s8, c8) = fullAdder c7 a7 b7
          (s9, c9) = fullAdder c8 a6 b6
          (s10, c10) = fullAdder c9 a5 b5
          (s11, c11) = fullAdder c10 a4 b4
          (s12, c12) = fullAdder c11 a3 b3
          (s13, c13) = fullAdder c12 a2 b2
          (s14, c14) = fullAdder c13 a1 b1
          (s15, _) = fullAdder c14 a0 b0

inc16 :: Bus16Input -> Bus16Output
inc16 i = add16 i bus16One
