module Sequential.RAM where

import Sequential.DFF (Sequential, read, write, output, state);
import Sequential.Registers (Load, register)

import Bus.Data (Bus16)
import Bus.Gates (Bus16Input, Bus16Output, Selector3, mux8Way16, dmux8Way)

type Memory m = Sequential m Bus16Output

data Memory8 = Memory8 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16 Bus16

ram8 :: Bus16Input -> Selector3 -> Load -> Memory Memory8
ram8 bus sel load
    = do
        -- current state
        (Memory8 m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l7, l6, l5, l4, l3, l2, l1, l0) = dmux8Way sel load

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
        let o = mux8Way16 o7 o6 o5 o4 o3 o2 o1 o0 sel
        output o


data Memory64 = Memory64 Memory8 Memory8 Memory8 Memory8 Memory8 Memory8 Memory8 Memory8

ram64 :: Bus16Input -> (Selector3, Selector3) -> Load -> Memory Memory64
ram64 bus (sel1, sel2) load
    = do
        -- current state
        (Memory64 m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l7, l6, l5, l4, l3, l2, l1, l0) = dmux8Way sel1 load

        let (o0, s0) = state (ram8 bus sel2 l0) m0
        let (o1, s1) = state (ram8 bus sel2 l1) m1
        let (o2, s2) = state (ram8 bus sel2 l2) m2
        let (o3, s3) = state (ram8 bus sel2 l3) m3
        let (o4, s4) = state (ram8 bus sel2 l4) m4
        let (o5, s5) = state (ram8 bus sel2 l5) m5
        let (o6, s6) = state (ram8 bus sel2 l6) m6
        let (o7, s7) = state (ram8 bus sel2 l7) m7

        write (Memory64 s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o7 o6 o5 o4 o3 o2 o1 o0 sel1
        output o


data Memory512 = Memory512 Memory64 Memory64 Memory64 Memory64 Memory64 Memory64 Memory64 Memory64

ram512 :: Bus16Input -> (Selector3, Selector3, Selector3) -> Load -> Memory Memory512
ram512 bus (sel1, sel2, sel3) load
    = do
        -- current state
        (Memory512 m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l7, l6, l5, l4, l3, l2, l1, l0) = dmux8Way sel1 load

        let (o0, s0) = state (ram64 bus (sel2, sel3) l0) m0
        let (o1, s1) = state (ram64 bus (sel2, sel3) l1) m1
        let (o2, s2) = state (ram64 bus (sel2, sel3) l2) m2
        let (o3, s3) = state (ram64 bus (sel2, sel3) l3) m3
        let (o4, s4) = state (ram64 bus (sel2, sel3) l4) m4
        let (o5, s5) = state (ram64 bus (sel2, sel3) l5) m5
        let (o6, s6) = state (ram64 bus (sel2, sel3) l6) m6
        let (o7, s7) = state (ram64 bus (sel2, sel3) l7) m7

        write (Memory512 s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o7 o6 o5 o4 o3 o2 o1 o0 sel1
        output o


data Memory4K = Memory4K Memory512 Memory512 Memory512 Memory512 Memory512 Memory512 Memory512 Memory512

ram4K :: Bus16Input -> (Selector3, Selector3, Selector3, Selector3) -> Load -> Memory Memory4K
ram4K bus (sel1, sel2, sel3, sel4) load
    = do
        -- current state
        (Memory4K m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l7, l6, l5, l4, l3, l2, l1, l0) = dmux8Way sel1 load

        let (o0, s0) = state (ram512 bus (sel2, sel3, sel4) l0) m0
        let (o1, s1) = state (ram512 bus (sel2, sel3, sel4) l1) m1
        let (o2, s2) = state (ram512 bus (sel2, sel3, sel4) l2) m2
        let (o3, s3) = state (ram512 bus (sel2, sel3, sel4) l3) m3
        let (o4, s4) = state (ram512 bus (sel2, sel3, sel4) l4) m4
        let (o5, s5) = state (ram512 bus (sel2, sel3, sel4) l5) m5
        let (o6, s6) = state (ram512 bus (sel2, sel3, sel4) l6) m6
        let (o7, s7) = state (ram512 bus (sel2, sel3, sel4) l7) m7

        write (Memory4K s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let o = mux8Way16 o7 o6 o5 o4 o3 o2 o1 o0 sel1
        output o



data Memory16K = Memory16K Memory4K Memory4K Memory4K Memory4K Memory4K Memory4K Memory4K Memory4K

ram16K :: Bus16Input -> (Selector3, Selector3, Selector3, Selector3, Selector3) -> Load -> Memory Memory16K
ram16K bus (sel1, sel2, sel3, sel4, sel5) load
    = do
        -- current state
        (Memory16K m0 m1 m2 m3 m4 m5 m6 m7) <- read

        -- write
        let (l7, l6, l5, l4, l3, l2, l1, l0) = dmux8Way sel1 load

        let (o0, s0) = state (ram4K bus (sel2, sel3, sel4, sel5) l0) m0
        let (o1, s1) = state (ram4K bus (sel2, sel3, sel4, sel5) l1) m1
        let (o2, s2) = state (ram4K bus (sel2, sel3, sel4, sel5) l2) m2
        let (o3, s3) = state (ram4K bus (sel2, sel3, sel4, sel5) l3) m3
        let (o4, s4) = state (ram4K bus (sel2, sel3, sel4, sel5) l4) m4
        let (o5, s5) = state (ram4K bus (sel2, sel3, sel4, sel5) l5) m5
        let (o6, s6) = state (ram4K bus (sel2, sel3, sel4, sel5) l6) m6
        let (o7, s7) = state (ram4K bus (sel2, sel3, sel4, sel5) l7) m7

        write (Memory16K s0 s1 s2 s3 s4 s5 s6 s7)

        -- read
        let out = mux8Way16 o7 o6 o5 o4 o3 o2 o1 o0 sel1
        output out
