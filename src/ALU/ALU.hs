module ALU.ALU where

import Bit.Gates
import Bus.Data
import Bus.Gates
import ALU.Adders

aluBasic :: Bus16Input -> Bus16Input -> Selector -> Selector -> Selector -> Selector -> Selector -> Selector -> Bus16Output
aluBasic x y zx nx zy ny f no = gateNo
    where gateX1 = mux16 x bus16Zero zx
          gateX2 = mux16 gateX1 (not16 gateX1) nx
          gateY1 = mux16 y bus16Zero zy
          gateY2 = mux16 gateY1 (not16 gateY1) ny
          gateF = mux16 (and16 gateX2 gateY2) (add16 gateX2 gateY2) f
          gateNo = mux16 gateF (not16 gateF) no

alu :: Bus16Input -> Bus16Input -> Selector -> Selector -> Selector -> Selector -> Selector -> Selector -> (Bus16Output, Output, Output)
alu x y zx nx zy ny f no = (basic, not (or8Way p1 `or` or8Way p2), ng)
    where basic = aluBasic x y zx nx zy ny f no
          (p1, p2) = bus16ToBus8 basic
          ng = msb basic
