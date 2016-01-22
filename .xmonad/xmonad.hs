{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Loggers

import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace (onWorkspace)

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import Data.Ratio ((%))
import qualified Data.Map as M

main = xmonad =<< xmobar myConfig

-- myConfig = ewmh defaultConfig
myConfig = defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig
        -- , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        , modMask = mod4Mask
        , startupHook = setWMName "LG3D"
        , layoutHook = myLayoutHook
        , keys = \conf -> myKeys conf `M.union` keys defaultConfig conf
        , terminal = "sakura"
        }

myManageHook = composeAll
  [ className =? "Xfce4-notifyd" --> doIgnore
  , manageDocks
  , manageSpawn
  , isFullscreen --> doFullFloat
  , className =? "mpv" --> doFloat
  ]

myLayoutHook = onWorkspace "9" imLayout $ avoidStruts $ smartBorders $ standardLayouts
  where
    standardLayouts = tiled ||| Mirror tiled ||| Grid ||| noBorders Full
    -- Tall (windows in master pane) (resize increment) (master pane size)
    tiled = ResizableTall 1 (3/100) (1/2) []
    imLayout = avoidStruts $ smartBorders $ withIM (1%8) pidginRoster $ reflectHoriz $ withIM (1%8) skypeRoster (Mirror Grid ||| Grid ||| tiled ||| Mirror tiled)
    skypeRoster = And (ClassName "Skype") (Title "patcarlisle - Skype™")
    --skypeRoster = (ClassName "Skype") `And` (Title "patcarlisle - Skype™") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
    pidginRoster  = And (ClassName "Pidgin") (Role "buddy_list")

myStartupHook = do
  spawnOn "9" "pidgin"
  spawnOn "7" "chromium"
  spawnOn "8" "sakura"
  spawnOn "3" "sakura"
  spawnOn "6" "/home/patrick/bin/hipchat"
  spawn "nm-applet"

winMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
    , ((modm, xK_p), spawn "exe=`yeganesh -x -- -fn 'Consolas' -nb '#222222'` && eval \"exec $exe\"")
    , ((modm, xK_Print), spawn "xfce4-screenshooter -rc")
    , ((modm, xK_c), spawn "xfce4-screenshooter -rc")
    ]


-- width of screen is 1920 no it's not
dzenCommand = "dzen2 -e 'onstart=lower' -w 2000 -ta l -fg '#a8a3f7' -bg '#3f3c6d'"
-- myStatus :: XConfig a -> IO (XMonad.Layout.LayoutModifier.ModifiedLayout AvoidStruts a)
myStatus conf = statusBar dzenCommand myPP toggleStrutsKey conf

myPP = dzenPP { ppExtras = [padL battery] }

-- Helper function which provides ToggleStruts keybinding
-- taken from DynamicLog source
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
