import XMonad
import System.Exit

-- LAYOUTS
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing

-- WINDOW RULES
import XMonad.ManageHook

-- KEYBOARD & MOUSE RULES
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Util.EZConfig
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- STATUS BAR
import System.IO (hPutStrLn)
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize as FlexibleResize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
--import XMonad.Util.Dmenu
import XMonad.Util.Run (spawnPipe)

-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------

main = do
        xmproc <- spawnPipe "xmobar"
        xmonad $ ewmh defaultConfig
                { terminal              = myTerminal
                , borderWidth           = myBorderWidth
                , normalBorderColor     = colBorderNormal
                , focusedBorderColor    = colBorderFocused
                , modMask               = myModMask
                , keys                  = myKeys
                , workspaces            = myWorkspaces
                , focusFollowsMouse     = myFocusFollowsMouse
                , startupHook           = do
                                                setWMName "LG3D"
                                                -- spawn ("nitrogen --restore")
                , manageHook            = (doF W.swapDown) <+> newManageHook
                , layoutHook            = myLayout
                , handleEventHook       = fullscreenEventHook
                , logHook               = do
                                                raiseFocused
                                                dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
                }

-------------------------------------------------------------------------------
-- SETTINGS
-------------------------------------------------------------------------------

myTerminal              = "urxvt"
myBorderWidth           = 2
myModMask               = mod4Mask
myFocusFollowsMouse     = False


-------------------------------------------------------------------------------
-- COLOURS
-------------------------------------------------------------------------------

background              = "#222222"
colBorderNormal         = "#444444"
colBorderFocused        = "#09802c"
colWSEmpty              = "#8f8f8f"
colWSFocused            = "#09ba55"
colWSOccupied           = "#e0e0e0"
colLayout               = "#e0e0e0"

-------------------------------------------------------------------------------
-- WINDOW RULES
-------------------------------------------------------------------------------

myManageHook = composeAll       [ className =? "MPlayer" --> doFloat
                                , className =? "feh"    --> doFloat
                                , className =? "VLC"    --> doFloat
                                , className =? "Chromium" --> doShift (myWorkspaces !! 1)
                                , className =? "Firefox" --> doShift (myWorkspaces !! 1)
                                , resource =? "Dialog" --> doFloat
                                , resource =? "Splash" --> doFloat
                                , title =? "FLOAT" --> doFloat
                                , title =? "MAIL" --> doCenterFloat
                                ]

newManageHook = myManageHook <+> manageHook defaultConfig 

-------------------------------------------------------------------------------
-- WORKSPACES
-------------------------------------------------------------------------------

myWorkspaces = [ "I" , "II" , "III" , "IV" , "V" , "VI"]

-------------------------------------------------------------------------------
-- LAYOUTS
-------------------------------------------------------------------------------

myLayout = onWorkspace (myWorkspaces !! 0) (avoidStruts (tiledGap ||| tiledSpace ||| tiled) ||| fullTile)
                $ onWorkspace (myWorkspaces !! 1) (avoidStruts tiledGap ||| fullTile)
                $ onWorkspaces [(myWorkspaces !! 2), (myWorkspaces !! 5)] (avoidStruts simplestFloat)
                $ avoidStruts (tiledGap ||| tiled) ||| fullTile
        where
                tiled           = spacing 3 $ ResizableTall nmaster delta ratio []
                tiledGap        = gaps [(U,50), (R,50), (D,50), (L,50)] $ tiled
                tiledSpace      = spacing 50 $ ResizableTall nmaster delta ratio []
                fullScreen      = noBorders(fullscreenFull Full)
                fullTile        = spacing 3 $ ResizableTall nmaster delta ratio []
                borderlessTile  = noBorders(fullTile)
                nmaster         = 1
                delta           = 5/100
                ratio           = toRational (2/(1 + sqrt 5 :: Double))

-------------------------------------------------------------------------------
-- STATUS BAR
-------------------------------------------------------------------------------

myPP = xmobarPP
        { ppCurrent             = xmobarColor colWSFocused background . pad
        , ppVisible             = xmobarColor colBorderFocused background . pad
        , ppHidden              = xmobarColor colWSOccupied background . pad
        , ppHiddenNoWindows     = xmobarColor colWSEmpty background . pad
        , ppWsSep               = ""
        , ppSep                 = "    "
        , ppLayout              = xmobarColor colLayout background .
                (\x -> case x of
                        "Full"                          -> "ÿ"
                        "Spacing 3 ResizableTall"       -> "þ"
                        "Spacing 50 ResizableTall"      -> "þ"
                        "ResizableTall"                 -> "þ"
                        "SimplestFloat"                 -> "ý"
                        _                               -> "ú"
                )
        , ppOrder               = \(ws:l:_) -> [ws,l]
        }

-------------------------------------------------------------------------------
-- KEYBOARD
-------------------------------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	[((mod4Mask			, xK_Return), spawn "urxvt")
	,((mod4Mask .|. shiftMask	, xK_Return), spawn "~/bin/emet")
	,((mod4Mask			, xK_t), spawn "~/bin/ranger_spawn.sh")
	,((mod4Mask			, xK_p), spawn "~/bin/dmenu.sh")
	,((mod4Mask .|. shiftMask	, xK_p), spawn "~/bin/dmenu_custom.bash")
	,((mod4Mask .|. shiftMask	, xK_w), spawn "chromium")
	,((mod4Mask  			, xK_w), spawn "firefox")
	,((mod4Mask .|. controlMask	, xK_c), kill)
        ,((modMask                      , xK_Tab), windows W.focusDown)
        ,((modMask                      , xK_j), windows W.focusDown)
        ,((modMask                      , xK_k), windows W.focusUp)
        ,((modMask                      , xK_m), windows W.focusMaster)
        ,((modMask .|. shiftMask        , xK_j), windows W.swapDown)
        ,((modMask .|. shiftMask        , xK_k), windows W.swapUp)
	,((mod4Mask  			, xK_e), moveTo Next EmptyWS)
	,((mod4Mask			, xK_c), windows W.focusDown)
	,((mod4Mask .|. shiftMask	, xK_c), windows W.focusUp)
        ,((modMask                      , xK_comma), sendMessage (IncMasterN 1))
        ,((modMask                      , xK_period), sendMessage (IncMasterN (-1)))
        ,((modMask                      , xK_space), sendMessage NextLayout)
        ,((modMask .|. shiftMask        , xK_space), setLayout $ XMonad.layoutHook conf)
        ,((modMask                      , xK_h), sendMessage Shrink)
        ,((modMask                      , xK_l), sendMessage Expand)
	,((mod4Mask .|. shiftMask	, xK_l), sendMessage MirrorShrink)
	,((mod4Mask .|. shiftMask	, xK_h), sendMessage MirrorExpand)
	,((mod4Mask  			, xK_a), withFocused (keysMoveWindow (-20,0)))
	,((mod4Mask  			, xK_s), withFocused (keysMoveWindow (0,-20)))
	,((mod4Mask  			, xK_d), withFocused (keysMoveWindow (0,20)))
	,((mod4Mask  			, xK_f), withFocused (keysMoveWindow (20,0)))
	,((mod4Mask .|. shiftMask  	, xK_a), withFocused (keysResizeWindow (-20,0) (0,0)))
	,((mod4Mask .|. shiftMask  	, xK_s), withFocused (keysResizeWindow (0,-20) (0,0)))
	,((mod4Mask .|. shiftMask  	, xK_d), withFocused (keysResizeWindow (0,20) (0,0)))
	,((mod4Mask .|. shiftMask  	, xK_f), withFocused (keysResizeWindow (20,0) (0,0)))
	,((mod4Mask			, xK_m), spawn "~/.xmonad/scripts/menu ~/.xmonad/apps")
        ,((mod4Mask .|. shiftMask       , xK_Escape), spawn "sudo systemctl suspend")
        ,((modMask                      , xK_q), spawn "xmonad --recompile && xmonad --restart")
        ,((mod4Mask .|. shiftMask       , xK_q), io (exitWith ExitSuccess))
        ]
        ++
        -- mod-[1..9] %! Switch to workspace N
        -- mod-shift-[1..9] %! Move client to workspace N
        [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]




-------------------------------------------------------------------------------
-- MOUSE
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CUSTOM FUNCTIONS
-------------------------------------------------------------------------------

-- raise window on focus
raiseFocused :: X ()
raiseFocused = do
        disp <- asks display
        mw <- gets (W.peek . windowset)
        maybe (return ()) (io . (raiseWindow disp)) mw

