module Lib
  ( outputGrid,
    formatGrid,
    findWord,
    findWordInLine,
    findWords,
    getLines,
    skew,
    zipOverGrid,
    zipOverGridWith,
    gridWithCoords,
  )
where

import Data (grid, languages)
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)

type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let rows = map repeat [0 ..]
      cols = repeat [0 ..]
   in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

formatGrid :: Grid Cell -> String
formatGrid grid =
  let charGrid = (map . map) cell2char grid
   in unlines charGrid

cell2char :: Cell -> Char
cell2char (Cell _ c) = c

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

--
getLines :: Grid Char -> [String]
getLines grid =
  -- look for a word from l to r on grid
  let horizontal = grid
      -- to find a word from t to b and b to t we transpose the rows and cols
      vertical = transpose grid
      -- we use the skew function to recursively add characters to the grid
      -- this will make it so we can find words that are diagonally
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in -- We include the reversed lines and we can search both l to r and r to l
      lines ++ map reverse lines

diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skew

-- write a function that skews the grid by pre-pending characters to it
skew :: Grid Char -> Grid Char
skew [] = []
skew (l : ls) = l : skew (map indent ls)
  where
    indent line = '_' : line

-- check for word in every line in every direction
findWord :: Grid Char -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = any (findWordInLine word) lines
   in if found then Just word else Nothing

-- Get a list of only values (use Hoogle [Maybe a]-> [a])
findWords :: Grid Char -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid) words
   in catMaybes foundWords

-- find a word on a single line (Hoogle)
findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf
