module Util.Binary where

import Prelude (Show, Eq, Bool (True, False), show, (==))

import Data.Text.Lazy (Text)

import Binary (Binary (One, Zero))

instance Eq Binary where
    One == One = True
    Zero == Zero = True
    _ == _ = False

instance Show Binary where
    show Zero = "0"
    show One  = "1"

bit :: Text -> Binary
bit "0" = Zero
bit "1" = One
