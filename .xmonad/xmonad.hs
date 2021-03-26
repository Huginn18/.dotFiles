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
import XMonad.Actions.Volume

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
import XMonad.Hooks.UrgencyHook

-- --- -- - -- ---
-- IMPORTS: UTILS
-- --- -- - -- ---
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedWindows
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

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

-- --- -- - -- ---
-- HOOKS
-- --- -- - -- ---
myStartupHook = do
    spawnOnce   "nitrogen --restore &"
    spawnOnce   "compton &"
    spawnOnce   "spotify"
-- --- -- - -- --- -- - -- ---
    spawnOnce   "keepassxc"
    spawnOnce   "signal-desktop"
    spawnOnce   "discord"
-- --- -- - -- --- -- - -- ---
    spawnOnce   "firefox"
    spawnOnce   "~/.config/bin/initAudioVolume.sh"
-- --- -- - -- --- -- - -- --- -- - -- --- -- - -- ---
myLogHook xmobar = dynamicLogWithPP $ defaultPP
    { 
    --how to print the tag of the currently focused workspace
      ppCurrent         = xmobarColor "#ebdbb2" "" . wrap "[" "]" 
    --how to print tags of invisible workspaces which contain windows
    , ppHidden          = xmobarColor "#a89984" "" . wrap "*" ""
    --how to print tags of empty invisible workspaces
    , ppHiddenNoWindows = xmobarColor "#665c54" ""
    --format to be applied to tags of urgent workspaces
    , ppUrgent          = xmobarColor "#ff00ff" "" . wrap "!" "!"
    --separator to use between different log sections
    , ppSep             = "<fc=#665c54> | </fc>" 
    -- separator to use between workspace tags
    , ppWsSep           =  "<fc=#665c54> | </fc>"
    --window title format
    , ppTitle           = xmobarColor "#665c54" "" . shorten 32
    --layout name format
    , ppLayout          = xmobarColor "#00ff00" ""
    --how to order the different log sections
    , ppOrder           = \(ws:t:ex) -> [ws]
    --, ppExtras = 

    , ppOutput = \x -> hPutStrLn xmobar x
    }

-- --- -- - -- --- -- - -- --- -- - -- --- -- - -- ---
myManageWindowsHook = composeAll
    [
      className =? "Signal"     --> doShift "8: im"
    , className =? "keepassxc"  --> doShift "9: pass"
    , className =? "Firefox"    --> doShift "2: www"
    , className =? "discord"    --> doShift "8: im"
    , className =? "UnityHub"   --> doShift "4: unity"
    , className =? "jetbrains-toolbox" --> doShift "3: dev"
    , className =? "Unity"      --> doShift "4: unity"
    ]
-- --- -- - -- ---
-- LAYOUT
-- --- -- - -- ---
myLayoutHook = avoidStruts $ spacing 6 $ layoutSet
    where

        -- --- -- - --
        -- variables
        -- --- -- - --
        nMaster = 1     -- number of windows in master column
        ratio12 = 1/2   
        delta = 3/100   -- percent of screen to increment when resizing

        --
        tiledLayout = Tall nMaster delta ratio12
        layoutGrid = Grid
        layoutTall = Tall nMaster delta ratio12
        layoutSet = toggleLayouts Full (layoutTall ||| layoutGrid ||| Full)

-- --- -- - -- --- 
-- WORKSPACES
-- --- -- - -- --- 
myWorkspaces = ["1: music", "2: www", "3: dev", "4: unity", "5", "6", "7", "8: im", "9: pass"]

-- --- -- - -- ---
-- HOTKEYS
-- --- -- - -- ---
myKeys :: [(String, X ())]
myKeys = 
    [ ("M-S-q", io (exitWith ExitSuccess))
    , ("M-S-c", kill)
    , ("M-S-l", spawn "xscreensaver-command -l")
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

    , ("M-f", withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1))
    -- Workspace Controls
    , ("M-<R>", nextWS)
    , ("M-<L>", prevWS) 
    
    -- Volume Control
    , ("<XF86AudioMute>",toggleMute >> return())
    , ("<XF86AudioLowerVolume>",lowerVolume 5 >> return())
    , ("M-<D>",lowerVolume 5 >> return())
    , ("<XF86AudioRaiseVolume>",raiseVolume 5 >> return())
    , ("M-<U>",raiseVolume 5 >> return())
    
    -- Music Control
    , ("M-S-<L>", spawn "playerctl previous")
    , ("M-S-<R>", spawn "playerctl next")
    , ("M-S-<Space>", spawn "playerctl play-pause")
    ]

-- --- -- - -- --- -- - -- ---
-- MAIN
-- --- -- - -- --- -- - -- ---
main = do
    xmobar  <- spawnPipe "xmobar -x 0 /home/huginn/.config/xmobar/xmobarrc"   
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh def
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
        , manageHook = myManageWindowsHook
        }`additionalKeysP` myKeys 
