module Bus.Data where

import Bit.Data (Binary (Zero, One))

data Bus8 = Bus8 Binary Binary Binary Binary Binary Binary Binary Binary

data Bus16 = Bus16 Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary

msb :: Bus16 -> Binary
msb (Bus16 a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = a

bus16ToBus8 :: Bus16 -> (Bus8, Bus8)
bus16ToBus8 (Bus16 i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15) = (Bus8 i0 i1 i2 i3 i4 i5 i6 i7, Bus8 i8 i9 i10 i11 i12 i13 i14 i15)

bus16Zero :: Bus16
bus16Zero = Bus16 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero

bus16One :: Bus16
bus16One = Bus16 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero One
