import           XMonad                            hiding ((|||))
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleManipulate as Flex
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
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

------------------------------------------------------------------------
-- vars
--
altKey          = mod1Mask
winKey          = mod4Mask
numLockKey      = mod2Mask
dzenFont        = "-xos4-terminus-bold-r-normal-*-12-*-*-*-*-*-iso8859-15"
colBG           = "#0f0f0f"
colHidden       = "#555555"
colFocus        = "#0099ff"
colNormal       = "#ffffff"
colUrgent       = "#ff0000"
colBorderNormal = "#dddddd"
colBorderFocus  = "#AA0033"

------------------------------------------------------------------------
-- Key bindings
--

newKeyBindings x = M.union (M.fromList . keyBindings $ x) (keys defaultConfig x)
keyBindings conf@(XConfig {XMonad.modMask = modMask}) =
  addKeyBinding modMask xK_p (spawn "synapse") $ -- launch action; old="prog=`dmenu_run` && eval \"exec ${prog}\""

  addKeyBinding cModShift xK_n refresh $ -- Resize viewed windows to the correct size

  addKeyBinding modMask xK_Right (windows W.focusDown) $ -- Move focus to the next / previous window
  addKeyBinding modMask xK_Left  (windows W.focusUp  ) $ -- Move focus to the next / previous window
  addKeyBinding cModShift xK_m (windows W.swapMaster) $ -- Swap the focused window and the master window
  addKeyBinding modMask xK_m (windows W.swapDown) $ -- Swap the focused window with the next window
  addKeyBinding modMask xK_l (windows W.swapUp) $ -- Swap the focused window with the previous window

  addKeyBinding cCtrlAlt xK_l (mapM_ spawn ["xscreensaver -no-splash", "xscreensaver-command -lock"]) $ -- screensaver

  addKeyBinding modMask xK_Down (sendMessage Shrink) $ -- Shrink the master area
  addKeyBinding modMask xK_Up (sendMessage Expand) $ -- Expand the master area
  addKeyBinding modMask xK_f (sendMessage ToggleLayout) $ -- set window fullscreen
  addKeyBinding cModCtrlShift xK_space (sendMessage resetAlt) $ -- Reset the layout

  addKeyBinding 0 0x1008FF11 (spawn "amixer set Master 4-") $ --volume keys: Down
  addKeyBinding 0 0x1008FF13 (spawn "amixer set Master 4+") $ --volume keys: Up
  addKeyBinding 0 0x1008FF12 (spawn "amixer set Master toggle") $ --volume keys: Mute
  --addKeyBinding modMask xK_Print (spawn "exe=`gnome-screenshot` && eval \"exec $exe\"") $
  
  addKeyBinding cCtrlAlt      xK_Left  prevWS      $ -- Switch workspaces (and move windows) horizontally
  addKeyBinding cCtrlAlt      xK_Right nextWS      $ -- Switch workspaces (and move windows) horizontally
  --addKeyBinding cModCtrl      xK_Up    toggleWS    $
  --addKeyBinding cModCtrl      xK_Down  toggleWS    $
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
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w)) -- Set the window to floating mode and move by dragging
      , ((modMask, button2), (\w -> focus w >> windows W.swapMaster)) -- Raise the window to the top of the stack
      , ((modMask, button3), ((\w -> focus w >> Flex.mouseWindow Flex.resize w))) -- Performs only a resize of the window, based on which quadrant the mouse is in.
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
 
myLayout = (toggleLayouts $ avoidStruts full) $ smartBorders $ onWorkspace "F5:chat" imLayout standardLayouts
  where
    standardLayouts = wide ||| tall ||| full ||| grid
    tall     = named "tall"   $ avoidStruts basic
    wide     = named "wide"   $ avoidStruts $ Mirror basic
    grid     = named "grid"   $ avoidStruts Grid
    full     = named "full"   $ avoidStruts $ noBorders Full

    -- IM layout (http://pbrisbin.com/posts/xmonads_im_layout)
    imLayout = named "im"     $ avoidStruts $ withIM (1%8) skypeRoster standardLayouts
    csshLayout = named "cssh"     $ avoidStruts $ withIM (1%5) csshMaster standardLayouts
    --withIM (1%9) pidginRoster $ reflectHoriz $ --reflectHoriz put the roster on the right

    csshMaster = ClassName "Cssh" 
    skypeRoster  = ClassName "Skype"  `And` Title "excilys_agoulamhoussen - Skype™"
 
------------------------------------------------------------------------
-- Window rules:
--
myManageHook :: ManageHook
myManageHook =  manageDocks <+> (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] 
    , [className    =? c            --> doShift  "1:www"    |   c   <- myWebs   ] 
    , [className    =? c            --> doShift  "2:IDE"    |   c   <- myIDEs   ] 
    , [className    =? c            --> doShift  "F2:vault" |   c   <- myVault  ] 
    , [className    =? c            --> doShift  "F5:chat"  |   c   <- myChat   ] 
    --, [className    =? c            --> doShift  "6:gimp"   |   c   <- myGimp   ] 
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] 
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] 
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where
 
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        -- classnames
        myFloats  = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser"]
        myIDEs    = ["jetbrains-pycharm","jetbrains-idea"]
        myMovie   = ["Boxee","Trine"]
        myMusic	  = ["Rhythmbox","Spotify","MPlayer"]
        myChat	  = ["Pidgin","Buddy List","Skype"]
        myGimp	  = ["Gimp"]
        myDev	  = ["gnome-terminal"]
        myVim	  = ["Gvim"]
        myVault   = ["Keepassx","Keepass"]
 
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
 
        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]
 
-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat


------------------------------------------------------------------------
-- Status bars and logging


myStatusBar = "dzen2 -m -x 0 -y 0 -h 24 -w 900 -ta l -fg '" ++ colNormal ++ "' -bg '" ++ colBG ++ "' -fn '" ++ dzenFont ++ "'"
-- myDzenRight = "/home/alnour/.xmonad/scripts/loop.sh | dzen2 -fn '" ++ dzenFont ++ "' -x 1230 -y 0 -h 24 -w 510 -ta r -bg '" ++ colBG ++ "' -fg '" ++ colNormal ++ "' -p -e ''"
myDzenRight = "conky -c ~/.xmonad/conky.conf | dzen2 -fn '" ++ dzenFont ++ "' -x 900 -y 0 -h 24 -w 968 -ta r -bg '" ++ colBG ++ "' -fg '" ++ colNormal ++ "' -p -e ''"


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
  dzen <- spawnPipe myStatusBar
  dzenRight <- spawn myDzenRight
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ defaultConfig    
    { 
      terminal           = "urxvt",
      focusFollowsMouse  = True,
      borderWidth        = 2,
      modMask            = winKey,
      --numlockMask        = numLockKey,
      workspaces         = ["1:www", "2:IDE", "3", "4", "5","F1","F2:vault","F3","F4","F5:chat"],
      normalBorderColor  = colBorderNormal,
      focusedBorderColor = colBorderFocus,
      keys               = newKeyBindings,
      mouseBindings      = myMouseBindings,
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = dynamicLogWithPP defaultPP
        {
          ppCurrent = dzenColor colFocus colBG,
          ppVisible = dzenColor colNormal colBG,
          ppHiddenNoWindows = dzenColor colHidden "",
          ppUrgent = dzenColor colUrgent "",
          ppTitle = dzenColor colNormal "" . wrap "< " " >",
          ppWsSep = " ",
          ppSep = " ^i(.xmonad/icons/separator.xbm) ",
          ppOutput = hPutStrLn dzen
        },
      startupHook        = do
        -- LG3D is needed for java-based apps to avoid (some of) focus issues
        setWMName "LG3D"
        spawn "~/.xmonad/startup-hook"
    }
