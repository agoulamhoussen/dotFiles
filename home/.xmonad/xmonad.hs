import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.Grid
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.IM
import XMonad.Config.Gnome
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByTag)

------------------------------------------------------------------------
-- Func
--
addKeyBinding shortcutLeft shortcutRight action xs = ((shortcutLeft, shortcutRight), action) : xs

------------------------------------------------------------------------
-- vars
--
altKey          = mod1Mask
winKey          = mod4Mask
numLockKey      = mod2Mask
dzenFont        = "-adobe-*-bold-r-normal-*-12-*-*-*-*-*-iso8859-1"
colBG           = "#0f0f0f"
colHidden       = "#555555"
colFocus        = "#0099ff"
colNormal       = "#ffffff"
colBorderNormal = "#dddddd"
colBorderFocus  = "#AA0033"

dmenuCommandBasic    = "dmenu -p '>' -l 10 -nf '" ++ colNormal  ++ "' -nb '" ++ colBG ++ "' -fn '"++ dzenFont  ++"' -sb '"++ colFocus ++"' -sf '"++ colNormal  ++"'"
-- dmenuCommand         = "prog=`dmenu_run | " ++ dmenuCommandBasic  ++ "` && eval \"exec ${prog}\""
dmenuCommand         = "prog=`dmenu_run` && eval \"exec ${prog}\""


------------------------------------------------------------------------
-- Key bindings
--

newKeyBindings x = M.union (M.fromList . keyBindings $ x) (keys gnomeConfig x)
keyBindings conf@(XConfig {XMonad.modMask = modMask}) =
  addKeyBinding cModShift xK_p (sendMessage (IncMasterN 1))   $
  addKeyBinding cModShift xK_o (sendMessage (IncMasterN (-1))) $
  -- launch a terminal (default)
  -- addKeyBinding modMask xK_Return (spawn $ XMonad.terminal conf) $
  -- launch dmenu
  addKeyBinding modMask xK_p (spawn dmenuCommandBasic) $
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
  addKeyBinding cCtrlAlt xK_l (mapM_ spawn ["gnome-screensaver-command -l"]) $
  -- Shrink the master area
  addKeyBinding modMask xK_Down (sendMessage Shrink) $
  -- Expand the master area
  addKeyBinding modMask xK_Up (sendMessage Expand) $
  -- set window fullscreen
  addKeyBinding modMask xK_f (sendMessage ToggleLayout) $
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
 
myLayout = (toggleLayouts $ avoidStruts full) $ smartBorders $ onWorkspace "10:skype" imLayout standardLayouts
  where
    standardLayouts = wide ||| tall ||| full ||| circle
    tall     = named "tall"   $ avoidStruts basic
    wide     = named "wide"   $ avoidStruts $ Mirror basic
    circle   = named "circle" $ avoidStruts circleSimpleDefaultResizable
    full     = named "full"   $ noBorders Full
 
   -- IM layout (http://pbrisbin.com/posts/xmonads_im_layout)
    imLayout =  named "im"    $ avoidStruts $ withIM (1%9) pidginRoster $ reflectHoriz $
                                              withIM (1%8) skypeRoster standardLayouts
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"
    skypeRoster  = ClassName "Skype"  `And` Role "MainWindow"
 

------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , title =? "GNU Image Manipulation Program" --> doFloat
    , title =? "GIMP"                 --> doFloat
    , className =? "Do"               --> doIgnore
    , className =? "Tilda"            --> doFloat
    , title     =? "VLC media player" --> doFloat
--    , className =? "Firefox"          --> doF (W.shift $ myWorkspaces!!0 )
--    , className =? "Iceweasel"        --> doF (W.shift $ myWorkspaces!!0 )
    ]
        <+> manageDocks
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
  xmonad $ gnomeConfig
    { 
      terminal           = "urxvt",
      focusFollowsMouse  = True,
      borderWidth        = 2,
      modMask            = winKey,
      --numlockMask        = numLockKey,
      workspaces         = ["1:web","2:term","3:IDE","4:sublime","5:gitg","6:server","7:keepassx","8","9","10:skype"],
      normalBorderColor  = colBorderNormal,
      focusedBorderColor = colBorderFocus,
      keys               = newKeyBindings,
      mouseBindings      = myMouseBindings,
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = takeTopFocus, -- (from ICCCMFocus) needed to avoid (some of) java-based apps focus issues
      startupHook        = setWMName "LG3D" -- LG3D is needed for java-based apps to avoid (some of) focus issues
    }
