module MyScratchpads
(myScratchpads)
where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicProperty
import XMonad.Util.NamedScratchpad
import Options (myTerminal)

myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" spawnTerm queryTerm manageTerm
                ]
  where
    spawnTerm   = myTerminal ++ " -t scratchpad -e tmux"
    queryTerm   = title =? "scratchpad"
    manageTerm  = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

-- Note: It's not worth trying to float spotify: it does not play along nicely.
