{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Random
import System.Environment
import Data.List
import Data.Char (toLower)
import Brick
import Brick.Widgets.Center
import Brick.Util (on)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border
import Graphics.Vty

-- APP LOGIC
-- words we prefer not to obscure
commonWords :: [String]
commonWords = ["the", "and"]

-- how many times we'll try to not obscure a common word
-- in a line of all common words, after 5 tries we'll just
-- pick one
maxTries :: Int
maxTries = 5

data Poem = Poem
  { title :: String
  , body :: [String]
  , obscured :: Bool
  } deriving (Show, Eq)

makePoem :: [String] -> Bool -> Poem
makePoem [] _ = Poem "" [] False
makePoem xs isObscured = Poem (head xs) (fmap preserveBlankLine $ tail xs) isObscured
  where
    preserveBlankLine "" = " "
    preserveBlankLine l = l

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

obscureLine :: Maybe Int -> String -> String
obscureLine Nothing l = l
obscureLine (Just n) l =
  let
    ws = words l
    wsWithIndex = withIndex ws
  in
    unwords $ map helper wsWithIndex
  where
    helper (idx, wrd)
      | idx == n = obscureWord wrd
      | otherwise = wrd
      
idxesToHide :: [String] -> IO [Maybe Int]
idxesToHide body = do
  mapM ((hideHelper 0) . words) body

hideHelper :: Int -> [String] -> IO (Maybe Int)
hideHelper _ [] = return Nothing
hideHelper tries line = do
  idx <- randomRIO (0, (length line - 1))
  let isCommonWord = (fmap toLower (line !! idx)) `elem` commonWords
  if isCommonWord && (tries < maxTries)
    then hideHelper (tries + 1) line
    else return (Just idx)

file2Poem :: FilePath -> IO Poem
file2Poem file = do
  s <- readFile file
  ls <- return $ lines s
  return $ makePoem ls False

wordObscuredPoem :: Poem -> IO Poem
wordObscuredPoem poem = do
  let pBody = body poem
  idxes <- idxesToHide pBody
  obscBody <- return $ zipWith (\n l -> obscureLine n l) idxes pBody
  return $ poem { body = obscBody, obscured = True }

lineObscuredPoem :: Poem -> IO Poem
lineObscuredPoem p@(Poem _ [] _) = return p
lineObscuredPoem poem = do
  idx <- randomRIO (0, maxIdx poem)
  return $ poem { body = fmap (obsc idx) (withIndex $ body poem)}
  where
    maxIdx :: Poem -> Int
    maxIdx p = (length $ body p) - 1

    obsc :: Int -> (Int, String) -> String
    obsc j (i, line)
      | i == j = unwords $ fmap obscureWord $ words line
      | otherwise = line

-- UI
renderPoem :: Poem -> Widget ()
renderPoem poem =
  let
    titleClass = if (obscured poem) then "obscuredTitle" else "title"
    tWidget = withAttr titleClass $ hCenter $ str $ title poem
    bWidget = vBox $ fmap (hCenter . str) (body poem)
    combined = [tWidget, bWidget]
  in
    center $ vBox combined

renderPoems :: [Poem] -> [Widget ()]
renderPoems ps = [border $ hBox $ intersperse vBorder $ fmap renderPoem ps]

aMap :: AttrMap
aMap = attrMap (brightWhite `on` black)
  [ ("title", cyan `on` black)
  , ("obscuredTitle", magenta `on` black)
  ]

app :: App [Poem] e ()
app = App
  { appDraw = renderPoems
  , appChooseCursor = neverShowCursor
  , appHandleEvent = resizeOrQuit
  , appStartEvent = return
  , appAttrMap = const aMap
  }

displayPoem :: (Poem -> IO Poem) -> FilePath -> IO ()
displayPoem f file = do
  p <- file2Poem file
  p2 <- f p
  _ <- defaultMain app [p2, p]
  return ()

wordMode :: FilePath -> IO ()
wordMode = displayPoem wordObscuredPoem

lineMode :: FilePath -> IO ()
lineMode = displayPoem lineObscuredPoem

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> wordMode file
    ["word", file] -> wordMode file
    ["line", file] -> lineMode file
    _ -> putStrLn "Usage: poem-thing [word|line] <filepath>"
