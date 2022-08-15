module Main where

import Data (grid, languages)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Lib
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (newStdGen)

getList :: IO [String]
getList = fmap reverse (go []) where
  go xs = do
    x <- getLine
    if x == "enter" then return xs
                    else go (x:xs)

main :: IO ()
main = do
  -- languages <- getLine
  -- if languages == "enter" then return []
  --                  else fmap (languages:) getList
  gen <- newStdGen
  let filledInGrid = fillInBlanks gen grid
      game = makeGame filledInGrid languages
  hSetBuffering stdout NoBuffering
  playTurn game

playTurn game = do
  putStrLn . formatGame $ game
  putStr "Please enter a word> "
  word <- getLine
  let newGame = playGame game word
  if completed newGame
    then putStrLn "Congratulations, you found all the words!"
    else playTurn newGame

