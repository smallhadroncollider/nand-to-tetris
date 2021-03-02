module Sequential.RAM where

import Sequential.DFF (Sequential, read, write, output, state);
import Sequential.Registers (Load, register)

import Bit.Gates (Selector)
import Bus.Data (Bus16)
import Bus.Gates (Bus16Input, Bus16Output, mux8Way16, dmux8Way)

type Memory m = Sequential m Bus16Output

data Memory8 = Memory8 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16

ram8 :: Bus16Input -> (Selector, Selector, Selector) -> Load -> Memory Memory8
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


data Memory64 = Memory64 Memory8 Memory8 Memory8 Memory8 Memory8 Memory8 Memory8 Memory8

ram64 :: Bus16Input -> (Selector, Selector, Selector, Selector, Selector, Selector) -> Load -> Memory Memory64
ram64 bus (a, b, c, e, f, g) load
    = do
        -- current state
        (Memory64 m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l0, l1, l2, l3, l4, l5, l6, l7) = dmux8Way a b c load

        let (o0, s0) = state (ram8 bus (e, f, g) l0) m0
        let (o1, s1) = state (ram8 bus (e, f, g) l1) m1
        let (o2, s2) = state (ram8 bus (e, f, g) l2) m2
        let (o3, s3) = state (ram8 bus (e, f, g) l3) m3
        let (o4, s4) = state (ram8 bus (e, f, g) l4) m4
        let (o5, s5) = state (ram8 bus (e, f, g) l5) m5
        let (o6, s6) = state (ram8 bus (e, f, g) l6) m6
        let (o7, s7) = state (ram8 bus (e, f, g) l7) m7

        write (Memory64 s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o0 o1 o2 o3 o4 o5 o6 o7 a b c
        output o


data Memory512 = Memory512 Memory64 Memory64 Memory64 Memory64 Memory64 Memory64 Memory64 Memory64

ram512 :: Bus16Input -> (Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector) -> Load -> Memory Memory512
ram512 bus (a, b, c, e, f, g, h, i, j) load
    = do
        -- current state
        (Memory512 m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l0, l1, l2, l3, l4, l5, l6, l7) = dmux8Way a b c load

        let (o0, s0) = state (ram64 bus (e, f, g, h, i, j) l0) m0
        let (o1, s1) = state (ram64 bus (e, f, g, h, i, j) l1) m1
        let (o2, s2) = state (ram64 bus (e, f, g, h, i, j) l2) m2
        let (o3, s3) = state (ram64 bus (e, f, g, h, i, j) l3) m3
        let (o4, s4) = state (ram64 bus (e, f, g, h, i, j) l4) m4
        let (o5, s5) = state (ram64 bus (e, f, g, h, i, j) l5) m5
        let (o6, s6) = state (ram64 bus (e, f, g, h, i, j) l6) m6
        let (o7, s7) = state (ram64 bus (e, f, g, h, i, j) l7) m7

        write (Memory512 s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o0 o1 o2 o3 o4 o5 o6 o7 a b c
        output o


data Memory4K = Memory4K Memory512 Memory512 Memory512 Memory512 Memory512 Memory512 Memory512 Memory512

ram4K :: Bus16Input -> (Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector) -> Load -> Memory Memory4K
ram4K bus (a, b, c, e, f, g, h, i, j, k, l, m) load
    = do
        -- current state
        (Memory4K m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l0, l1, l2, l3, l4, l5, l6, l7) = dmux8Way a b c load

        let (o0, s0) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l0) m0
        let (o1, s1) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l1) m1
        let (o2, s2) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l2) m2
        let (o3, s3) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l3) m3
        let (o4, s4) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l4) m4
        let (o5, s5) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l5) m5
        let (o6, s6) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l6) m6
        let (o7, s7) = state (ram512 bus (e, f, g, h, i, j, k, l, m) l7) m7

        write (Memory4K s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o0 o1 o2 o3 o4 o5 o6 o7 a b c
        output o



data Memory16K = Memory16K Memory4K Memory4K Memory4K Memory4K Memory4K Memory4K Memory4K Memory4K

ram16K :: Bus16Input -> (Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector, Selector) -> Load -> Memory Memory16K
ram16K bus (a, b, c, e, f, g, h, i, j, k, l, m, n, o, p) load
    = do
        -- current state
        (Memory16K m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l0, l1, l2, l3, l4, l5, l6, l7) = dmux8Way a b c load

        let (o0, s0) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l0) m0
        let (o1, s1) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l1) m1
        let (o2, s2) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l2) m2
        let (o3, s3) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l3) m3
        let (o4, s4) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l4) m4
        let (o5, s5) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l5) m5
        let (o6, s6) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l6) m6
        let (o7, s7) = state (ram4K bus (e, f, g, h, i, j, k, l, m, n, o, p) l7) m7

        write (Memory16K s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let out = mux8Way16 o0 o1 o2 o3 o4 o5 o6 o7 a b c
        output out
