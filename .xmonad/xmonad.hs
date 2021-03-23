import System.Exit


-- --- -- - -- --- -- - -- --- -- - -- ---
-- IMPORTS: XMonad
-- --- -- - -- --- -- - -- --- -- - -- ---
import XMonad
import qualified XMonad.StackSet as W

-- --- -- - -- --- -- - -- --- -- - -- ---
-- IMPORTS: ACTIONS
-- --- -- - -- --- -- - -- --- -- - -- ---
import XMonad.Actions.CycleWS

-- --- -- - -- --- -- - -- --- -- - -- ---
-- IMPORTS: LAYOUT
-- --- -- - -- --- -- - -- --- -- - -- ---
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.Grid

-- --- -- - -- --- -- - -- --- -- - -- ---
-- IMPORTS: HOOKS
-- --- -- - -- --- -- - -- --- -- - -- ---
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

-- --- -- - -- ---
-- IMPORTS: UTILS
-- --- -- - -- ---
import XMonad.Util.EZConfig
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


myLogHook xmobar = dynamicLogWithPP $ defaultPP
    { 
    --how to print the tag of the currently focused workspace
      ppCurrent         = xmobarColor "#ff0000" "" . wrap "[" "]" 
    --how to print tags of invisible workspaces which contain windows
    , ppHidden          = xmobarColor "#00ff00" "" . wrap "*" ""
    --how to print tags of empty invisible workspaces
    , ppHiddenNoWindows= xmobarColor "#0000ff" ""
    --format to be applied to tags of urgent workspaces
    , ppUrgent          = xmobarColor "#ff00ff" "" . wrap "!" "!"
    --separator to use between different log sections
    , ppSep             = "<fc=#f0000f> | </fc>" 
    -- separator to use between workspace tags
    , ppWsSep           = ""
    --window title format
    , ppTitle           = xmobarColor "#ff0000" "" . shorten 32
    --layout name format
    , ppLayout          = xmobarColor "#00ff00" ""
    --how to order the different log sections
    , ppOrder           = \(ws:t:ex) -> [ws]++ex++[t]
    --, ppExtras = 

    , ppOutput = \x -> hPutStrLn xmobar x
    }
-- --- -- - -- ---
-- LAYOUT
-- --- -- - -- ---
myLayoutHook = avoidStruts $ layoutSet
    where

        -- --- -- - --
        -- variables
        -- --- -- - --
        nMaster = 1     -- number of windows in master column
        ratio12 = 1/2   
        delta = 3/100   -- percent of screen to increment when resizing

        --
        tiledLayout = spacing 8 $ Tall nMaster delta ratio12
        layoutGrid = spacing 8 $ Grid
        layoutTall = Tall nMaster delta ratio12
        layoutSet = toggleLayouts Full (layoutTall ||| layoutGrid ||| Full)

-- --- -- - -- --- 
-- WORKSPACES
-- --- -- - -- --- 
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- --- -- - -- ---
-- HOTKEYS
-- --- -- - -- ---
myKeys :: [(String, X ())]
myKeys = 
    [ ("M-S-q", io (exitWith ExitSuccess))
    , ("M-S-c", kill)
    , ("M-q", spawn "xmonad --recompile; killall xmobar; xmonad --restart")
    , ("M-p", spawn "dmenu_run")
    , ("M-S-<Return>", spawn(myTerminal))
 
    -- Layout Controls
    , ("M-<Return>", windows W.swapMaster)
    , ("M-b", sendMessage ToggleStruts)
    
    , ("M-k", windows W.focusUp)
    , ("M-j", windows W.focusDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-j", windows W.swapDown)

    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    
    , ("M-C-k", sendMessage (IncMasterN 1))
    , ("M-C-j", sendMessage (IncMasterN (-1))) 

    -- Workspace Controls
    , ("M-<R>", nextWS)
    , ("M-<L>", prevWS) 
   ]

-- --- -- - -- --- -- - -- ---
-- MAIN
-- --- -- - -- --- -- - -- ---
main = do
    xmobar  <- spawnPipe "xmobar -x 0 /home/huginn/.config/xmobar/xmobarrc"   

    xmonad $ ewmh def
        { borderWidth = myBorderWidth 
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask = mod4Mask -- Use Super instead of Alt
        , terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse

        , workspaces = myWorkspaces
          -- hooks
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , handleEventHook = docksEventHook
        , logHook = myLogHook xmobar
        }`additionalKeysP` myKeys 
