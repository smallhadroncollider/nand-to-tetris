module Sequential.DFF
    ( dff
    , get
    , put
    , return
    , runState
    , Sequential
    ) where

import Control.Applicative ((<*))
import Control.Monad.State.Strict (State, get, put, return, runState)

import Bit.Gates (Input, Output)

type Sequential a s = State a s

dff :: Input -> State Output Output
dff value = get <* put value
