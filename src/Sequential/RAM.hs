module Sequential.RAM where

import Sequential.DFF (Sequential, read, write, output, state);
import Sequential.Registers (Load, register)

import Bit.Gates (Selector)
import Bus.Data (Bus16)
import Bus.Gates (Bus16Input, Bus16Output, mux8Way16, dmux8Way)

data Memory8 = Memory8 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16

ram8 :: Bus16Input -> (Selector, Selector, Selector) -> Load -> Sequential Memory8 Bus16Output
ram8 bus (a, b, c) load
    = do
        -- current state
        (Memory8 m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l0, l1, l2, l3, l4, l5, l6, l7) = dmux8Way a b c load

        let (o0, s0) = state (register bus l0) m0
        let (o1, s1) = state (register bus l1) m1
        let (o2, s2) = state (register bus l2) m2
        let (o3, s3) = state (register bus l3) m3
        let (o4, s4) = state (register bus l4) m4
        let (o5, s5) = state (register bus l5) m5
        let (o6, s6) = state (register bus l6) m6
        let (o7, s7) = state (register bus l7) m7

        write (Memory8 s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o0 o1 o2 o3 o4 o5 o6 o7 a b c
        output o
