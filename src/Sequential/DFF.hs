module Sequential.DFF
    ( dff
    , read
    , write
    , output
    , state
    , Sequential
    ) where

import Control.Applicative ((<*))
import Control.Monad.State.Strict (MonadState, State, get, put, return, runState)

import Bit.Gates (Input, Output)

type Sequential s a = State s a

output :: a -> Sequential s a
output = return

read :: MonadState s m => m s
read = get

write :: MonadState s m => s -> m ()
write = put

state :: Sequential s a -> s -> (a, s)
state = runState

dff :: Input -> State Output Output
dff value = read <* write value
