module Sequential.DFF where

import Control.Applicative ((<*))
import Control.Monad.State.Strict (State, get, put)

import Bit.Gates (Input, Output)

type Sequential a s = State a s

dff :: Input -> State Output Output
dff value = get <* put value
