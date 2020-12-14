{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module MyLayoutHook
(myLayoutHook)
where

import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import Options

-- Gaps around and between windows
-- Changes only seem to apply if I log out then in again
-- Dimensions are given as (Border top bottom right left)
mySpacing = spacingRaw True                -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 0 15 10 10) -- Size of screen edge gaps
                       True                -- Enable screen edge gaps
                       (Border 5 5 5 5)    -- Size of window gaps
                       True                -- Enable window gaps

myTabConfig = def { fontName            = myFont
                  , activeColor         = myTabActiveColour
                  , inactiveColor       = myTabInactiveColour
                  , activeBorderColor   = myTabActiveBorderColour
                  , inactiveBorderColor = myTabInactiveBorderColour
                  , activeTextColor     = myTabActiveTextColour
                  , inactiveTextColor   = myTabInactiveTextColour
                  }

tall  = renamed [Replace "Tall"]
      $ mySpacing
      $ avoidStruts
      $ ResizableTall 1 (3/100) (1/2) []

three = renamed [Replace "Three"]
      $ mySpacing
      $ avoidStruts
      $ ThreeColMid 1 (3/100) (1/2)

tabs  = renamed [Replace "Tabs"]
      $ avoidStruts
      $ tabbed shrinkText myTabConfig

myLayoutHook = smartBorders
--           $ mySideDecorate  -- Messes up everything - I don't yet understand why
             $ mkToggle (single NBFULL)
             $ tall ||| three ||| tabs
