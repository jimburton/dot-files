import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig ( additionalKeys )
import qualified XMonad.StackSet as W
import XMonad.Hooks.SetWMName

import System.Exit
import Control.Monad.IO.Class
import qualified Data.Map as M


myFocusedBorderColour = "#FFFFFF"

--myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
     [  ((modMask, xK_p)                   , spawn "dmenu_run")
      , ((modMask .|. shiftMask, xK_q)     , spawn "kill -9 -1")
      , ((modMask .|. shiftMask, xK_Return), spawn "gnome-terminal")
      , ((modMask .|. shiftMask, xK_c)     , kill)
      , ((modMask, xK_q)                   , restart "xmonad" True)
      , ((modMask, xK_space)               , sendMessage NextLayout)
      , ((modMask .|. shiftMask, xK_l)     , spawn "slock")
      , ((modMask .|. shiftMask, xK_s)     , spawn "systemctl suspend")
      , ((modMask, xK_n)                   , refresh)
      , ((modMask, xK_Tab)                 , windows W.focusDown)
      , ((modMask .|. shiftMask, xK_Tab)   , windows W.focusUp)
      , ((modMask, xK_Return)              , windows W.swapMaster)
      , ((modMask, xK_h)                   , sendMessage Shrink)
      , ((modMask, xK_l)                   , sendMessage Expand)
      , ((modMask, xK_t)                   , withFocused $ windows . W.sink)
      , ((modMask, xK_Up)                  , spawn "pactl set-sink-volume 0 +10%")
      , ((modMask, xK_Down)                , spawn "pactl set-sink-volume 0 -10%")
      , ((modMask, xK_0)                   , spawn "pactl set-sink-mute 0 toggle")
     ]
     ++
     [((m .|. modMask, k), windows $ f i)
     	  | (i,k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
	  , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myStartUpHook :: X ()
myStartUpHook = do spawn "xsetroot -solid chocolate"
	           spawn "stalonetray --icon-size=12 --kludges=force_icons_size"
		   spawn "dropbox start"
		   spawn "nm-applet"
                   spawn "pulseaudio -D"
                   setWMName "LG3D"

myManagementHooks :: [ManageHook]
myManagementHooks = [ resource =? "stalonetray" --> doIgnore
  		    ]

main = xmonad =<< xmobar gnomeConfig
              {   
                focusedBorderColor = myFocusedBorderColour
                , modMask          = mod4Mask
		, keys             = myKeys
		, startupHook      = docksStartupHook <+> myStartUpHook
		, manageHook       = manageDocks <+> manageHook defaultConfig <+> composeAll myManagementHooks
                , handleEventHook  = docksEventHook
              } 
