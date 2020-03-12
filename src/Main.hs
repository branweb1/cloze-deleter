{-# LANGUAGE OverloadedStrings #-}

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



-- chant :: [String]
-- chant =
--   [ "Bowl-washing Chant"
--   , "We use this water to wash our bowls"
--   , "it tastes like heavenly nectar."
--   , "We offer it to the many spirits to satisfy them."
--   , "Om, Maha-ku-sha-laya Svaha!"
--   ]

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile file
      ls <- return $ lines s
      p <- return $ makePoem ls False
      b <- return $ body p
      ns <- idxesToHide b
      os <- return $ zipWith (\n l -> obscureLine n l) ns b
      p2 <- return $ p { body = os, obscured = True }
      _ <- defaultMain app [p2, p]
      return ()
    _ -> putStrLn "wrong number of args"
