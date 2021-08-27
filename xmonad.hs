--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = xmonad =<< statusBar "xmobar" xmobarPP {
    ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
} (XMonad.modMask, xK_b) def {
-- simple stuff
  terminal           = "termonad",
  focusFollowsMouse  = False,
  clickJustFocuses   = True,
  borderWidth        = 1,
  modMask            = mod4Mask,
  workspaces         = ["1","2","3","4","5","6","7","8","9"],
  normalBorderColor  = "#5E5086",
  focusedBorderColor = "#ffffff",

-- key bindings
  keys               = myKeys,
  mouseBindings      = myMouseBindings,

-- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ],
  handleEventHook    = mempty,
  logHook            = return (),
  startupHook        = setWMName "LG3D"
}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

  -- launch a terminal
  [ ((XMonad.modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

  -- launch rofi
  , ((XMonad.modMask,               xK_p     ), spawn "rofi -show run")

  -- launch gmrun
  , ((XMonad.modMask .|. shiftMask, xK_p     ), spawn "gmrun")

  -- close focused window
  , ((XMonad.modMask .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
  , ((XMonad.modMask,               xK_space ), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default
  , ((XMonad.modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size
  , ((XMonad.modMask,               xK_n     ), refresh)

  -- Move focus to the next window
  , ((XMonad.modMask,               xK_Tab   ), windows W.focusDown)

  -- Move focus to the next window
  , ((XMonad.modMask,               xK_j     ), windows W.focusDown)

  -- Move focus to the previous window
  , ((XMonad.modMask,               xK_k     ), windows W.focusUp  )

  -- Move focus to the master window
  , ((XMonad.modMask,               xK_m     ), windows W.focusMaster  )

  -- Swap the focused window and the master window
  , ((XMonad.modMask,               xK_Return), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((XMonad.modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

  -- Swap the focused window with the previous window
  , ((XMonad.modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

  -- Shrink the master area
  , ((XMonad.modMask,               xK_h     ), sendMessage Shrink)

  -- Expand the master area
  , ((XMonad.modMask,               xK_l     ), sendMessage Expand)

  -- Push window back into tiling
  , ((XMonad.modMask,               xK_t     ), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((XMonad.modMask              , xK_comma ), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((XMonad.modMask              , xK_period), sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap
  -- Use this binding with avoidStruts from Hooks.ManageDocks.
  -- See also the statusBar function from Hooks.DynamicLog.
  --
  -- , ((XMonad.modMask              , xK_b     ), sendMessage ToggleStruts)

  -- Quit xmonad
  , ((XMonad.modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

  -- Restart xmonad
  , ((XMonad.modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

  -- Run xmessage with a summary of the default keybindings (useful for beginners)
  , ((XMonad.modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
  ]
  ++

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. XMonad.modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. XMonad.modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

  -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                     >> windows W.shiftMaster))

  -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                     >> windows W.shiftMaster))

  -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
