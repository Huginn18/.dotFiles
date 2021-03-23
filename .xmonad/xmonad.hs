import XMonad

-- --- -- - -- ---
-- Utils
-- --- -- - -- ---
import XMonad.Util.SpawnOnce

-- --- -- - -- ---
-- VARIABLES
-- --- -- - -- ---
myBorderWidth = 2

myNormalBorderColor = "#928374"
myFocusedBorderColor = "#ebdbb2"

myModMask :: KeyMask
myModMask = mod4Mask -- super/windows key | default: mod1Mask left alt

myTerminal :: String
myTerminal="alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- --- -- - -- ---
-- HOOKS
-- --- -- - -- ---
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "compton &"

-- --- -- - -- ---
-- HOTKEYS
-- --- -- - -- ---



-- --- -- - -- --- -- - -- ---
-- MAIN
main = xmonad defaultConfig
        {
          borderWidth = myBorderWidth 
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        , modMask = mod4Mask -- Use Super instead of Alt
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        -- hooks
        , startupHook = myStartupHook
        }

