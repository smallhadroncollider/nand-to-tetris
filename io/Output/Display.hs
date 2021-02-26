{-# LANGUAGE OverloadedStrings #-}
module Output.Display where

import Prelude (Int, (.), (<$>), (<>), (*), (+), (!!), map, zip)

import Data.Text (Text, concat)
import Data.List.Split (chunksOf)

import Bit.Data (Binary (Zero, One))

blocks :: [Text]
blocks = [ " " -- 0000
         , "▗" -- 0001
         , "▖" -- 0010
         , "▄" -- 0011
         , "▝" -- 0100
         , "▐" -- 0101
         , "▞" -- 0110
         , "▟" -- 0111
         , "▘" -- 1000
         , "▚" -- 1001
         , "▌" -- 1010
         , "▙" -- 1011
         , "▀" -- 1100
         , "▜" -- 1101
         , "▛" -- 1110
         , "█" -- 1111
         ]

flat :: ([Binary],[Binary]) -> [Binary]
flat (a, b) = a <> b

zipper :: [[[Binary]]] -> [([Binary], [Binary])]
zipper [a, b] = zip a b
zipper [a] = zip a a
zipper _ = []

transform :: [[Binary]] -> [[[Binary]]]
transform input = map flat . zipper . map (chunksOf 2) <$> chunksOf 2 input

toInt :: Binary -> Int
toInt Zero = 0
toInt One = 1

toDecimal :: [Binary] -> Int
toDecimal [a, b, c, d] = (toInt a * 8) + (toInt b * 4) + (toInt c * 2) + toInt d
toDecimal _ = 0

keys :: [[Binary]] -> [[Int]]
keys input = map toDecimal <$> transform input

output :: [[Binary]] -> [Text]
output input = concat . map (blocks !!) <$> keys input
