module Main where

import Data (grid, languages)
import Lib

main :: IO ()
main =
  let gwc = gridWithCoords grid
   in outputGrid gwc
