module Lib
  ( outputGrid,
    formatGrid,
    findWord,
    findWords,
    findWordInLine,
    findWordInCellLinePrefix,
    getLines,
    skew,
    zipOverGrid,
    zipOverGridWith,
    gridWithCoords,
    cell2char,
    Cell (Cell, Indent),
    Game (gameGrid, gameWords),
    makeGame,
    totalWords,
    score,
    completed,
    playGame,
    formatGame,
    formatGameGrid,
    makeRandomGrid,
    fillInBlanks,
  )
where

import Data (grid, languages)
import Data.Char
import Data.List (isInfixOf, transpose)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import System.Random (Random (randomRs), RandomGen (split))

data Game = Game
  { gameGrid :: Grid Cell,
    gameWords :: M.Map String (Maybe [Cell])
  }
  deriving (Show)

data Cell
  = Cell (Integer, Integer) Char
  | Indent
  deriving (Eq, Ord, Show)

type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
   in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
   in case foundWord of
        Nothing -> game
        Just cs ->
          let dict = gameWords game
              newDict = M.insert word foundWord dict
           in game {gameWords = newDict}

formatGame :: Game -> String
formatGame game =
  formatGameGrid game
    ++ "\n\n"
    ++ show (score game)
    ++ "/"
    ++ show (totalWords game)

makeRandomGrid gen =
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
   in row : makeRandomGrid gen2

fillInBlanks gen grid =
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _ = c
   in zipOverGridWith fill grid r

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let rows = map repeat [0 ..]
      cols = repeat [0 ..]
   in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGameGrid :: Game -> String
formatGameGrid game =
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell =
        let char = cell2char cell
         in if cell `elem` cellSet
              then char
              else toLower char
      charGrid = mapOverGrid formatCell grid
   in unlines charGrid

--
getLines :: Grid Cell -> [[Cell]]
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

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

-- write a function that skews the grid by pre-pending characters to it
skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l : ls) = l : skew (map indent ls)
  where
    indent line = Indent : line

-- check for word in every line in every direction
findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
   in listToMaybe (catMaybes foundWords)

-- Get a list of only values (use Hoogle [Maybe a]-> [a])
findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid) words
   in catMaybes foundWords

-- find a word on a single line (Hoogle)
findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
   in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x : xs) (c : cs)
  | x == cell2char c =
    findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing
