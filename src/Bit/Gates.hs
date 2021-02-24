module Bit.Gates
    ( nand
    , not
    , and
    , or
    , xor
    , mux
    , dmux
    , Selector
    , Output
    ) where

import Bit.Data (Binary (One, Zero))

type Input = Binary
type Output = Binary

-- primitive nand gate
nand :: Input -> Input -> Output
nand One One = Zero
nand _ _ = One


-- not gate
not :: Input -> Output
not value = value `nand` value

-- and gate
and :: Input -> Input -> Output
and a b = not (a `nand` b)

-- or gate
or :: Input -> Input -> Output
or a b = not a `nand` not b

-- xor gate
xor :: Input -> Input -> Output
xor a b = (a `and` not b) `or` (not a `and` b)


-- three input

type Selector = Binary

-- mux gate
mux :: Input -> Input -> Selector -> Output
mux a b sel = (not sel `and` a) `or` (sel `and` b)

dmux :: Selector -> Input -> (Output, Output)
dmux sel value = (not sel `and` value, sel `and` value)
