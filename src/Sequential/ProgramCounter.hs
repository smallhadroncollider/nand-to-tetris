module Sequential.ProgramCounter where

import Sequential.DFF (Sequential, read, write, output, state);
import Sequential.Registers (Load, register)
import Bit.Gates (Selector, mux, not)
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
        let res = mux16 input bus16Zero reset

        -- should increment
        let shouldInc = mux (mux increment (not load) load) (not reset) reset
        let res' = mux16 res (inc16 bus) shouldInc

        -- store in register
        let load' = mux (mux load reset reset) increment increment -- needs to load if resetting/incrementing
        let (out, st) = state (register res' load') bus

        -- update state monad
        write st
        output out
