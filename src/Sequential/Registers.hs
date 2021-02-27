module Sequential.Registers where

import Sequential.DFF (Sequential, dff, read, write, output, state);

import Bit.Gates (Input, Output, Selector, mux)
import Bus.Data (Bus16 (Bus16))
import Bus.Gates (Bus16Input, Bus16Output)

type Load = Selector

registerBit :: Input -> Load -> Sequential Output Output
registerBit value load = do
    old <- read
    dff (mux old value load)

register :: Bus16Input -> Load -> Sequential Bus16Output Bus16Output
register
    (Bus16 i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15)
    load
    = do
        (Bus16 ini0 ini1 ini2 ini3 ini4 ini5 ini6 ini7 ini8 ini9 ini10 ini11 ini12 ini13 ini14 ini15) <- read

        let (o0, s0) = state (registerBit i0 load) ini0
        let (o1, s1) = state (registerBit i1 load) ini1
        let (o2, s2) = state (registerBit i2 load) ini2
        let (o3, s3) = state (registerBit i3 load) ini3
        let (o4, s4) = state (registerBit i4 load) ini4
        let (o5, s5) = state (registerBit i5 load) ini5
        let (o6, s6) = state (registerBit i6 load) ini6
        let (o7, s7) = state (registerBit i7 load) ini7
        let (o8, s8) = state (registerBit i8 load) ini8
        let (o9, s9) = state (registerBit i9 load) ini9
        let (o10, s10) = state (registerBit i10 load) ini10
        let (o11, s11) = state (registerBit i11 load) ini11
        let (o12, s12) = state (registerBit i12 load) ini12
        let (o13, s13) = state (registerBit i13 load) ini13
        let (o14, s14) = state (registerBit i14 load) ini14
        let (o15, s15) = state (registerBit i15 load) ini15

        write (Bus16 s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15)
        output (Bus16 o0 o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15)
