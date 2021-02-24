module Bus.Data where

import Bit.Data (Binary (Zero, One))

data Bus8 = Bus8 Binary Binary Binary Binary Binary Binary Binary Binary

data Bus16 = Bus16 Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary Binary

bus16One :: Bus16
bus16One = Bus16 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero One
