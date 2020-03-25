{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Random
import System.Environment
import Data.List
import Data.Char (toLower)
import Brick
import Brick.Main as M
import Brick.Types as T
import Brick.Widgets.Center
import Brick.Util (on)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border
import Graphics.Vty as V

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
makePoem xs isObscured = Poem (head xs) (tail xs) isObscured

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

stanzatize :: [String] -> [[String]]
stanzatize [] = []
stanzatize lns@(l:ls) =
  let
    (stanza, rest) = span (\x -> x /= "") (if l == "" then ls else lns)
  in
    stanza : (stanzatize rest)

obscureStanza :: [String] -> IO [String]
obscureStanza xs = do
  idx <- randomRIO (0, maxIdx xs)
  return $ fmap (obsc idx) (withIndex xs)

maxIdx :: [String] -> Int
maxIdx ls = (length ls) - 1

obsc :: Int -> (Int, String) -> String
obsc j (i, line)
  | i == j = unwords $ fmap obscureWord $ words line
  | otherwise = line

lineObscuredPoem :: Poem -> IO Poem
lineObscuredPoem p@(Poem _ [] _) = return p
lineObscuredPoem poem = do
  let stanzas = stanzatize $ body poem
  obStanzas <- mapM obscureStanza stanzas
  pBody <- return $ concat $ intersperse [""] obStanzas
  return $ poem { body = pBody, obscured = True }

-- UI
renderPoem :: Poem -> Widget Name
renderPoem poem =
  let
    titleClass = if (obscured poem) then "obscuredTitle" else "title"
    tWidget = withAttr titleClass $ padTopBottom 2 $ str $ title poem
    bWidget = padBottom (Pad 2) $ str (unlines $ body poem)
    combined = [tWidget, bWidget]
  in
    hCenter $ vBox combined


renderPoems :: [Poem] -> [Widget Name]
renderPoems ps = [viewport ViewportMain Vertical $ vBox [hBox $ fmap renderPoem ps]]

aMap :: AttrMap
aMap = attrMap (brightWhite `on` black)
  [ ("title", brightBlue `on` black)
  , ("obscuredTitle", red `on` black)
  ]

data Name = ViewportMain deriving (Show, Ord, Eq)

vpScroll :: M.ViewportScroll Name
vpScroll = M.viewportScroll ViewportMain

appEvent :: [Poem] -> T.BrickEvent Name e -> T.EventM Name (T.Next [Poem])
appEvent p (T.VtyEvent (V.EvKey V.KDown  [])) = M.vScrollBy vpScroll 1 >> M.continue p
appEvent p (T.VtyEvent (V.EvKey (V.KChar ' ')  [])) = M.vScrollBy vpScroll 1 >> M.continue p
appEvent p (T.VtyEvent (V.EvKey V.KUp    [])) = M.vScrollBy vpScroll (-1) >> M.continue p
appEvent p _ = M.halt p

app :: App [Poem] e Name
app = App
  { appDraw = renderPoems
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEvent
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
