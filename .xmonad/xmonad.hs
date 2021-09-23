import XMonad
import Data.Char
import Data.Monoid
import System.Exit
import System.IO
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

cursorFColor = "#ffffff"
cursorBgColor = "#ffffff"
foregroundColor = "#eeffff"
backgroundColor = "#212121"
highlightFgColor = "#545454"
highlightBgColor = "#eeffff"
black = "#000000"
red = "#ff5370"
green = "#c3e88d"
yellow = "#ffcb6b"
blue = "#82aaff"
purple = "#c792ea"
cyan = "#89ddff"
white = "#ffffff"
lightBlack = "#545454"
lightRed = "#ff5370"
lightGreen = "#c3e88d"
lightYellow = "#ffcb6b"
lightBlue = "#82aaff"
lightPurple = "#c792ea"
lightCyan = "#89ddff"
lightWhite = "#ffffff"

workSpaces = ["<fn=8>\xf489</fn>","<fn=5>\xf268</fn>","<fn=7>\xfb0f</fn>","<fn=3>\xf121</fn>"]

main :: IO ()
main = do
    xmonad =<< statusBar "ghc --make .config/xmobar/xmobar.hs -dynamic -threaded && xmobar" myPP toggleStrutsKey (ewmh def
        { terminal           = "termonad"
        , focusFollowsMouse  = False
        , clickJustFocuses   = True
        , borderWidth        = 2
        , modMask            = mod4Mask
        , workspaces         = workSpaces
        , normalBorderColor  = cursorFColor
        , focusedBorderColor = cursorBgColor

        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        , layoutHook         = avoidStruts $ spacingRaw False (Border 5 5 15 15) True (Border 2 2 8 8) True $ reflectHoriz $ Tall 1 (1/20) (1/2) ||| Full
        , manageHook         = composeAll
            [ className =? "Google-chrome"  --> doShift (workSpaces !! 1)
            , className =? "Code"           --> doShift (workSpaces !! 2)
            , className =? "jetbrains-idea" --> doShift (workSpaces !! 3) ]
        , handleEventHook    = fullscreenEventHook
        , logHook            = return () >> setWMName "LG3D"
        , startupHook        = do
            spawnOnce "/usr/bin/numlockx on"
            spawnOnce "picom --experimental-backends"
            spawn "xwallpaper --daemon --zoom /etc/config/wallpapers/$[$RANDOM%`ls -l /etc/config/wallpapers | grep '^-' | wc -l`].jpg"
            spawnOnce "xsetroot -cursor_name left_ptr"
            spawnOnce "xfce4-panel"
            spawnOnce "jetbrains-toolbox --minimize"
            spawnOnce "xmonad --restart" })

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP
    { ppCurrent = xmobarColor foregroundColor "" . wrap ("<box type=Bottom width=2 mb=1 color="++foregroundColor++">") "</box>"
    , ppVisible = xmobarColor foregroundColor ""
    , ppHidden = xmobarColor purple "" . wrap ("<box type=Bottom width=2 mb=1 color="++purple++">") "</box>"
    , ppHiddenNoWindows = xmobarColor purple "" . wrap ("<fc="++purple++">") "</fc>"
    , ppTitle = xmobarColor cursorBgColor "" . shorten 60
    , ppSep = "<fc="++highlightFgColor++"> | </fc>"
    , ppUrgent = xmobarColor "#ffffff" "" . wrap "!" "!"
    , ppExtras = [gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset]
    , ppOrder = \(ws:l:t:ex) -> [ws]++ex++[t] }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi
    , ((modm,               xK_p     ), spawn "rofi -show run")

    , ((modm,               xK_x     ), spawn "jetbrains-toolbox; sleep 0.05; wmctrl -r 'Jetbrains toolbox' -e 0,22,$[`xrandr | grep current | awk '{print $10}' | cut -d , -f 1`-704],440,700")
    , ((modm,               xK_c     ), spawn "code")
    , ((modm,               xK_g     ), spawn "google-chrome-stable")
    , ((modm,               xK_e     ), spawn "Emacs")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

      -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Expand the master area
    , ((modm,               xK_h     ), sendMessage Expand)

    -- Shrink the master area
    , ((modm,               xK_l     ), sendMessage Shrink)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "pkill xwallpaper; xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")) ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

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
                                       >> windows W.shiftMaster)) ]

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
