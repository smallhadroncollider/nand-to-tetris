module Main where

import Prelude

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)
import Graphics.Vty

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Brick.BChan (BChan, newBChan, writeBChan)

import Bit.Data (Binary (Zero, One))
import Output.Display (output)


-- config
displayWidth :: Int
displayWidth = 512

displayHeight :: Int
displayHeight = 256

fps :: Int
fps = 1

delay :: Int
delay = 1000000 `div` fps

-- types
data Tick = Tick
type Name = ()

data State = State {
    position :: Int
  , direction :: Int
  , dimensions :: DisplayRegion
}

-- drawing
cols :: Int -> Int -> [Binary]
cols offset row = (\n -> if row < offset -5 || row > offset + 5 then Zero else One) <$> [1 .. displayWidth]

matrix :: Int -> [[Binary]]
matrix offset = cols offset <$> [1 .. displayHeight]

draw :: State -> [Widget Name]
draw state = [center $ border widget]
    where (w, h) = dimensions state
          width = w * 2
          height = (h - 2) * 2
          bigEnough = (width >= displayWidth) && (height >= displayHeight)
          screen = vBox (txt <$> output (matrix (position state)))
          widget = if bigEnough
              then screen
              else str ("Not big enough! (" <> show w  <> "/" <> show h <> ")")


-- state
getDimensions :: EventM Name DisplayRegion
getDimensions = (liftIO . displayBounds) . outputIface =<<  getVtyHandle

handleEvent :: State -> BrickEvent Name Tick -> EventM Name (Next State)
handleEvent state (VtyEvent (EvKey (KChar 'q') [])) = halt state
handleEvent state (VtyEvent (EvKey KEsc [])) = halt state
handleEvent state (VtyEvent (EvKey KUp [])) = continue $ state { direction = -1 }
handleEvent state (VtyEvent (EvKey KDown [])) = continue $ state { direction = 1 }
handleEvent state (AppEvent Tick) = do
    let pos = position state
    let dir = direction state
    continue $ state { position = pos + dir }
handleEvent state (VtyEvent (EvResize _ _)) = do
    dim <- getDimensions
    continue $ state { dimensions = dim }
handleEvent state _ = continue state

-- brick
app :: App State Tick Name
app = App {
    appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap defAttr [])
}

loop :: BChan Tick -> IO ()
loop chan = void . forkIO . forever $ do
    writeBChan chan Tick
    threadDelay delay

main :: IO ()
main = do
    chan <- newBChan 1
    loop chan
    let builder = mkVty defaultConfig
    vty <- builder
    bounds <- displayBounds (outputIface vty)
    void $ customMain vty builder (Just chan) app (State 0 1 bounds)
