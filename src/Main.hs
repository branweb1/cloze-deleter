module Main where

import System.Random
import System.Environment
import Data.List
import Data.Char (toLower)

commonWords :: [String]
commonWords = ["the", "and"]

maxTries :: Int
maxTries = 5

data Poem = Poem
  { title :: String
  , body :: [String]
  } deriving (Show, Eq)

makePoem :: [String] -> Poem
makePoem [] = Poem "" []
makePoem xs = Poem (head xs) (tail xs)

withIndex :: [a] -> [(Int, a)]
withIndex [] = []
withIndex xs =
  let
    lastIdx = (length xs) - 1
    idxs = [0..lastIdx]
  in
    zip idxs xs

obscureWord :: String -> String
obscureWord w =
  let
    n = length w
  in
    take n $ repeat '_'

obscureLine :: Int -> String -> String
obscureLine n l =
  let
    ws = words l
    wsWithIndex = withIndex ws
  in
    unwords $ map helper wsWithIndex
  where
    helper (idx, wrd)
      | idx == n = obscureWord wrd
      | otherwise = wrd
      
idxesToHide :: [String] -> IO [Int]
idxesToHide body = do
  mapM ((hideHelper 0) . words) body

hideHelper :: Int -> [String] -> IO Int
hideHelper tries line = do
  idx <- randomRIO (0, (length line - 1))
  let isCommonWord = (fmap toLower (line !! idx)) `elem` commonWords
  if isCommonWord && (tries < maxTries)
    then hideHelper (tries + 1) line
    else return idx

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      str <- readFile file
      l <- return $ lines str
      p <- return $ makePoem l
      b <- return $ body p
      ns <- idxesToHide b
      os <- return $ zipWith (\n l -> obscureLine n l) ns b
      putStrLn $ take 50 $ repeat '='
      putStr $ unlines $ intersperse "" os
      putStrLn $ take 50 $ repeat '='
      putStrLn "Press [Enter] to reveal text"
      putStrLn $ take 50 $ repeat '='      
      _ <- getLine
      putStr $ unlines $ intersperse "" b
      putStrLn $ take 50 $ repeat '='
    _ -> putStrLn "wrong number of args"
