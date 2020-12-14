{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module MyLayoutHook
(myLayoutHook)
where

import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.NoBorders            -- Remove borders when unnecessary (smartBorders)
import XMonad.Layout.Renamed              -- Rename layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing              -- Gaps around windows
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutModifier

-- Gaps around and between windows
-- Changes only seem to apply if I log out then in again
-- Dimensions are given as (Border top bottom right left)
mySpacing = spacingRaw True                -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 0 15 10 10) -- Size of screen edge gaps
                       True                -- Enable screen edge gaps
                       (Border 5 5 5 5)    -- Size of window gaps
                       True                -- Enable window gaps

tall  = renamed [Replace "Tall"]
      $ mySpacing
      $ ResizableTall 1 (3/100) (1/2) []

three = renamed [Replace "Three"]
      $ mySpacing
      $ ThreeColMid 1 (3/100) (1/2)

wide  = renamed [Replace "Wide"]
      $ Mirror tall

myLayoutHook =
              -- uncommenting either of the following two lines gives an ambiguous tyoe error
              avoidStruts
              $ smartBorders
--           $ mySideDecorate  -- Messes up everything - I don't yet understand why
              tall ||| three ||| wide ||| Full
