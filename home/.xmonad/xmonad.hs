import           XMonad                            hiding ((|||))
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ICCCMFocus
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators   (JumpToLayout (..), (|||))
import           XMonad.Layout.MosaicAlt
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ToggleLayouts
import           XMonad.Prompt
import qualified XMonad.StackSet                   as W
import           XMonad.Util.Run
import           XMonad.Util.WorkspaceCompare      (getSortByTag)
import           XMonad.Util.EZConfig              (additionalKeys)
import qualified Data.Map                          as M
import           Data.Ratio
import           System.Exit
import           System.IO                         (hPutStrLn)

------------------------------------------------------------------------
-- Func
--
addKeyBinding shortcutLeft shortcutRight action xs = ((shortcutLeft, shortcutRight), action) : xs

takeWorkspaces :: Int -> [String] -> [String]
takeWorkspaces = take

addWS' :: l -> W.StackSet [Char] l a sid sd -> W.StackSet [Char] l a sid sd
addWS' l s@(W.StackSet { W.hidden = ws }) = s { W.hidden = W.Workspace (show $ length (W.workspaces s) + 1) l Nothing:ws }

addWS :: X()
addWS = do l <- asks (layoutHook . config)
           windows (addWS' l) 

------------------------------------------------------------------------
-- vars
--
altKey          = mod1Mask
winKey          = mod4Mask
numLockKey      = mod2Mask
dzenFont        = "-xos4-terminus-bold-r-normal-*-12-*-*-*-*-*-iso8859-15"
iconSep         = ".xmonad/icons/separator.xbm"
colBG           = "#0f0f0f"
colHidden       = "#555555"
colFocus        = "#0099ff"
colNormal       = "#ffffff"
colUrgent       = "#ff0000"
colBorderNormal = "#dddddd"
colBorderFocus  = "#AA0033"
workspacesPool  = map show [1..]
myWorkspaces    = takeWorkspaces 10 workspacesPool

dmenuCommandBasic    = "dmenu -p '>' -l 10 -nf '" ++ colNormal  ++ "' -nb '" ++ colBG ++ "' -fn '"++ dzenFont  ++"' -sb '"++ colFocus ++"' -sf '"++ colNormal  ++"'"
-- dmenuCommand         = "prog=`dmenu_run | " ++ dmenuCommandBasic  ++ "` && eval \"exec ${prog}\""
dmenuCommand         = "prog=`dmenu_run` && eval \"exec ${prog}\""


------------------------------------------------------------------------
-- Key bindings
--

newKeyBindings x = M.union (M.fromList . keyBindings $ x) (keys defaultConfig x)
keyBindings conf@(XConfig {XMonad.modMask = modMask}) =
  addKeyBinding cModShift xK_p (sendMessage (IncMasterN 1))   $
  addKeyBinding cModShift xK_o (sendMessage (IncMasterN (-1))) $
  -- addKeyBinding cModShift xK_j (setLayout  $ XMonad.layoutHook conf)   $
  -- addKeyBinding cModShift xK_k (updateLayout W.current myLayout2  ) $
  -- launch a terminal (default)
  -- addKeyBinding modMask xK_Return (spawn $ XMonad.terminal conf) $
  -- launch dmenu
  addKeyBinding modMask xK_p (spawn dmenuCommand) $
  -- %! Push window back into tiling (default)
  -- addKeyBinding modMask xK_t (withFocused $ windows . W.sink)  $
  -- Resize viewed windows to the correct size
  addKeyBinding cModShift xK_n refresh $
  -- Move focus to the next / previous window
  addKeyBinding modMask xK_Right (windows W.focusDown) $
  addKeyBinding modMask xK_Left  (windows W.focusUp  ) $
  -- Swap the focused window and the master window
  addKeyBinding cModShift xK_m (windows W.swapMaster) $
  -- Swap the focused window with the next window
  addKeyBinding modMask xK_m (windows W.swapDown) $
  -- Swap the focused window with the previous window
  addKeyBinding modMask xK_l (windows W.swapUp) $
  -- screensaver
  addKeyBinding cCtrlAlt xK_l (mapM_ spawn ["xscreensaver -no-splash", "xscreensaver-command -lock"]) $
  -- Shrink the master area
  addKeyBinding modMask xK_Down (sendMessage Shrink) $
  -- Expand the master area
  addKeyBinding modMask xK_Up (sendMessage Expand) $
  -- set window fullscreen
  addKeyBinding modMask xK_f (sendMessage ToggleLayout) $
  --volume keys
  addKeyBinding 0 0x1008FF11 (spawn "amixer set Master 4-") $
  addKeyBinding 0 0x1008FF13 (spawn "amixer set Master 4+") $
  addKeyBinding 0 0x1008FF12 (spawn "amixer set Master toggle") $
  -- focus urgent window
  -- addKeyBinding modMask xK_u focusUrgent $
  -- Reset the layout
  addKeyBinding cModCtrlShift xK_space (sendMessage resetAlt) $
  --addKeyBinding modMask xK_Print (spawn "exe=`gnome-screenshot` && eval \"exec $exe\"") $
  -- Restart xmonad, does not work with old version
  --addKeyBinding modMask xK_q (mapM_ spawn ["pgrep -f loop.sh | xargs kill -9", "xmonad --recompile"]) $
  -- Switch workspaces (and move windows) horizontally
  addKeyBinding cCtrlAlt      xK_Left  prevWS      $
  addKeyBinding cCtrlAlt      xK_Right nextWS      $
  --addKeyBinding cModCtrl      xK_Up    toggleWS    $
  --addKeyBinding cModCtrl      xK_Down  toggleWS    $
  --addKeyBinding cModCtrlShift xK_Left  (shiftToPrev >> prevWS) $
  --addKeyBinding cModCtrlShift xK_Right (shiftToNext >> nextWS) $
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  ([((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
   ]
   ++
   [((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (workspaces conf) numAzerty,
         (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])
  where       
    cModCtrl      = modMask   .|. controlMask
    cModShift     = modMask   .|. shiftMask
    cCtrlShift    = shiftMask .|. controlMask
    cCtrlAlt      = altKey    .|. controlMask
    cModCtrlShift = cModCtrl  .|. shiftMask
    numAzerty       = [0x26,0xe9,0x22,0x27,0x28] ++ [xK_F1..xK_F12]
--    numAzerty       = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] ++ [xK_F1..xK_F12]

 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ 
      -- Set the window to floating mode and move by dragging
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
      -- Raise the window to the top of the stack
      , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
      -- Performs only a resize of the window, based on which quadrant the mouse is in. 
      , ((modMask, button3), ((\w -> focus w >> Flex.mouseWindow Flex.resize w)))
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
 
-- default tiling algorithm partitions the screen into two panes
basic :: Tall a
basic = Tall nmaster delta ratio
  where
    -- The default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Default proportion of screen occupied by master pane
    ratio   = 6/10
 
myLayout = (toggleLayouts $ avoidStruts full) $ smartBorders $ onWorkspace "F5:skype" imLayout standardLayouts
  where
    standardLayouts = wide ||| tall ||| full ||| grid
    tall     = named "tall"   $ avoidStruts basic
    wide     = named "wide"   $ avoidStruts $ Mirror basic
    grid     = named "grid"   $ avoidStruts Grid
    full     = named "full"   $ noBorders Full

    -- IM layout (http://pbrisbin.com/posts/xmonads_im_layout)
    imLayout = named "im"     $ avoidStruts $ withIM (1%8) skypeRoster standardLayouts
    csshLayout = named "cssh"     $ avoidStruts $ withIM (1%5) csshMaster standardLayouts
    --withIM (1%9) pidginRoster $ reflectHoriz $ --reflectHoriz put the roster on the right

    csshMaster = ClassName "Cssh" 
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
    skypeRoster  = ClassName "Skype"  `And` Title "excilys_agoulamhoussen - Skype™"
 
myLayout2 = (toggleLayouts $ avoidStruts full) $ smartBorders $ csshLayout 
  where
    standardLayouts = wide ||| tall ||| full ||| grid
    tall     = named "tall"   $ avoidStruts basic
    wide     = named "wide"   $ avoidStruts $ Mirror basic
    grid     = named "grid"   $ avoidStruts Grid
    full     = named "full"   $ noBorders Full

    -- IM layout (http://pbrisbin.com/posts/xmonads_im_layout)
    csshLayout = named "cssh"     $ avoidStruts $ withIM (1%8) csshMaster standardLayouts
    --withIM (1%9) pidginRoster $ reflectHoriz $ --reflectHoriz put the roster on the right
    csshMaster  = ClassName "Cssh"  
------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "MPlayer"                    --> doFloat
    , title =? "GNU Image Manipulation Program" --> doFloat
    , title =? "GIMP"                           --> doFloat
    , className =? "Do"                         --> doIgnore
    , className =? "Tilda"                      --> doFloat
    , title     =? "VLC media player"           --> doFloat
    , className =? "Firefox"                    --> doShift "1:www"
    , className =? "Skype"                      --> doShift "F5:skype"
    , className =? "Keepassx"                   --> doShift "F2:keepass"
    , className =? "jetbrains-pycharm"          --> doShift "3:IDE"
    , className =? "jetbrains-idea"             --> doShift "3:IDE"
    , resource  =? "desktop_window"             --> doIgnore
--    , className =? "Firefox"          --> doF (W.shift $ myWorkspaces!!0 )
--    , className =? "Iceweasel"        --> doF (W.shift $ myWorkspaces!!0 )
    ]
        <+> manageDocks

-- http://msscripting.com/2011/07/20/xmonad-part2/
-- {resource =? “desktop_window” –> doIgnore} kills any window named “desktop_window” and stops things like nautilus from forcing a desktop on me. 

------------------------------------------------------------------------
-- Status bars and logging
-- takeTopFocus is useful for java app focus
-- https://gist.github.com/markhibberd/636125/raw/11713d338e98a9dd5d126308218067a1628480df/xmonad-focus-wire.hs
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ICCCMFocus.html
myLogHook :: X()
myLogHook = takeTopFocus >> setWMName "LG3D" >> dynamicLogXinerama >> updatePointer (Relative 0.5 0.5)

myStartupHook = setWMName "LG3D"
myStatusBar = "dzen2 -m -x 0 -y 0 -h 24 -w 900 -ta l -fg '" ++ colNormal ++ "' -bg '" ++ colBG ++ "' -fn '" ++ dzenFont ++ "'"
-- myDzenRight = "/home/alnour/.xmonad/scripts/loop.sh | dzen2 -fn '" ++ dzenFont ++ "' -x 1230 -y 0 -h 24 -w 510 -ta r -bg '" ++ colBG ++ "' -fg '" ++ colNormal ++ "' -p -e ''"
myDzenRight = "conky -c ~/.xmonad/conky.conf | dzen2 -fn '" ++ dzenFont ++ "' -x 900 -y 0 -h 24 -w 968 -ta r -bg '" ++ colBG ++ "' -fg '" ++ colNormal ++ "' -p -e ''"

-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
  { ppCurrent = dzenColor colFocus colBG
  , ppVisible = dzenColor colNormal colBG
  , ppHiddenNoWindows = dzenColor colHidden ""
  , ppUrgent = dzenColor colUrgent ""
  , ppTitle = dzenColor colNormal "" . wrap "< " " >"
  , ppWsSep = " "
  , ppSep = " ^i(" ++ iconSep ++ ") "
  , ppOutput = hPutStrLn h
  }



------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
  dzen <- spawnPipe myStatusBar
  dzenRight <- spawnPipe myDzenRight
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ defaultConfig    
    { 
      terminal           = "urxvt",
      focusFollowsMouse  = True,
      borderWidth        = 2,
      modMask            = winKey,
      --numlockMask        = numLockKey,
      workspaces         = ["1:www", "2:term", "3:IDE", "4", "5","F1","F2:keepass","F3","F4","F5:skype"],
      normalBorderColor  = colBorderNormal,
      focusedBorderColor = colBorderFocus,
      keys               = newKeyBindings,
      mouseBindings      = myMouseBindings,
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = dynamicLogWithPP $ myDzenPP dzen, --takeTopFocus, -- (from ICCCMFocus) needed to avoid (some of) java-based apps focus issues
      startupHook        = setWMName "LG3D" -- LG3D is needed for java-based apps to avoid (some of) focus issues
    }
