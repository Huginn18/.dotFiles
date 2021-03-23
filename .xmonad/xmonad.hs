import XMonad

-- --- -- - -- ---
-- Utils
-- --- -- - -- ---
import XMonad.Util.SpawnOnce

-- --- -- - -- ---
-- VARIABLES
-- --- -- - -- ---
myModMask :: KeyMask
myModMask = mod4Mask -- super/windows key | default: mod1Mask left alt

myTerminal :: String
myTerminal="gnome-terminal"

-- --- -- - -- ---
-- HOOKS
-- --- -- - -- ---
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "compton &"

main = xmonad defaultConfig
        { 
          modMask = mod4Mask -- Use Super instead of Alt
        , terminal = myTerminal
        -- hooks
        , startupHook = myStartupHook
        }

