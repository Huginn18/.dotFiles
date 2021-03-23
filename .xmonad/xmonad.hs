import XMonad

-- --- -- - -- --- -- - -- --- -- - -- ---
-- IMPORTS: HOOKS
-- --- -- - -- --- -- - -- --- -- - -- ---
import XMonad.Hooks.EwmhDesktops

-- --- -- - -- ---
-- IMPORTS: UTIL
-- --- -- - -- ---
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

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
-- --- -- - -- --- -- - -- ---
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/huginn/.config/xmobar/xmobarrc"     
    xmonad $ ewmh def
        { borderWidth = myBorderWidth 
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        , modMask = mod4Mask -- Use Super instead of Alt
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
          -- hooks
        , startupHook = myStartupHook
        } 
