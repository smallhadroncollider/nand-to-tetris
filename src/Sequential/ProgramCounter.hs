module Sequential.ProgramCounter where

import Sequential.DFF (Sequential, read, write, output, state);
import Sequential.Registers (Load, register)
import Bit.Gates (Selector, not, or, and)
import Bus.Data (bus16Zero)
import Bus.Gates (Bus16Input, Bus16Output, mux16)
import ALU.Adders (inc16)

type Inc = Selector
type Reset = Selector

pc :: Bus16Input -> Inc -> Load -> Reset -> Sequential Bus16Output Bus16Output
pc input increment load reset = do
        -- current value
        bus <- read

        -- should reset
        let i1 = mux16 input bus16Zero reset

        -- should increment
        let i2 = mux16 i1 (inc16 bus) (increment `and` not (reset `or` load))

        -- store in register
        let load' = load `or` reset `or` increment
        let (out, st) = state (register i2 load') bus

        -- update state monad
        write st
        output out
