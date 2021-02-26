module Sequential.RAM where

import Sequential.DFF (Sequential, get, put, return, runState);
import Sequential.Registers (Load, register)
import Bit.Gates (Selector)
import Bus.Data (Bus16)
import Bus.Gates (Bus16Input, Bus16Output, mux8Way16, dmux8Way)

data Memory8 = Memory8 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16

ram8 :: Bus16Input -> (Selector, Selector, Selector) -> Load -> Sequential Memory8 Bus16Output
ram8 bus (a, b, c) load
    = do
        -- current state
        (Memory8 m0 m1 m2 m3 m4 m5 m6 m7) <- get

        -- write
        let (l0, l1, l2, l3, l4, l5, l6, l7) = dmux8Way a b c load

        let (_, s0) = runState (register bus l0) m0
        let (_, s1) = runState (register bus l1) m1
        let (_, s2) = runState (register bus l2) m2
        let (_, s3) = runState (register bus l3) m3
        let (_, s4) = runState (register bus l4) m4
        let (_, s5) = runState (register bus l5) m5
        let (_, s6) = runState (register bus l6) m6
        let (_, s7) = runState (register bus l7) m7

        put (Memory8 s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let m = mux8Way16 m0 m1 m2 m3 m4 m5 m6 m7 a b c
        return m
