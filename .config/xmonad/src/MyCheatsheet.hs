module MyCheatsheet
(myCheatsheet)
where

import Data.List.Split (chunksOf)
import System.IO
import Test.FitSpec.PrettyPrint (columns) -- Requires the 'fitspec' package
import XMonad
import XMonad.Util.NamedActions
import XMonad.Util.Run

-- Number of colomns with with which to display the cheatsheet
myCheatsheetCols :: Int
myCheatsheetCols = 3

-- Format the keybindings so they can be sent to the display
formatList :: [String] -> String
formatList list = columns "SeparatorPlaceholder" -- Normalise column widths -> Table
                $ map unlines -- Connect the sublists with line breals -> [column1,column2,...]
                $ chunksOf (myCheatsheetRows (list))
                $ list -- The list to be formatted

        where rowsFromColumns list nCol = 1 + length list `div` nCol
              myCheatsheetRows list = rowsFromColumns list myCheatsheetCols

-- How to display the cheatsheet (adapted from Ethan Schoonover's config)
myCheatsheet :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
myCheatsheet myKeyList = addName "Show Keybindings" $ io $ do
    handle <- spawnPipe "dzen2-display-cheatsheet"
    hPutStrLn handle "TitlePlaceholder\n" -- Replaced in the script
    hPutStrLn handle $ formatList (showKm myKeyList)
    hClose handle
    return ()
