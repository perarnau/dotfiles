--
-- xmonad example config file for xmonad-0.9
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- NOTE: Those updating from earlier xmonad versions, who use
-- EwmhDesktops, safeSpawn, WindowGo, or the simple-status-bar
-- setup functions (dzen, xmobar) probably need to change
-- xmonad.hs, please see the notes below, or the following
-- link for more details:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8
--

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile
import Graphics.X11.ExtraTypes
import System.IO
import Data.List
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtc"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1:term","2:web","3:mail","4:im","5:pdf","6:org","7:random","8:cal","9:wine","NSP"]

myFont = "-dejavu-dejavu sans mono-medium-r-normal-*-10-*-*-*-*-*-*"
-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ff0000"

myNormalFGColor = "#ffffff"
myNormalBGColor = "#000000"

myActiveTextColor = "#00ff00"

myUrgentTextColor = "#000000"
myUrgentBGColor = "#ff0000"

-- tabbed layout theme
myTabTheme = defaultTheme {
	activeColor = myNormalBGColor
	, inactiveColor = myNormalBGColor
	, urgentColor = myUrgentBGColor
	, activeBorderColor = myNormalFGColor
	, inactiveBorderColor = myNormalFGColor
	, urgentBorderColor = myUrgentTextColor
	, activeTextColor = myActiveTextColor
	, inactiveTextColor = myNormalFGColor
	, urgentTextColor = myUrgentTextColor
	, fontName = myFont
	}
-- Applications

myDmenuLaunch = "dmenu_run -nb black -nf white -sb gray -sf white -fn '-misc-fixed-medium-r-*-*-15-*-*-*-*-*-*-*'"
myTray = "pgrep stalonetray || stalonetray"
myXmobarLaunch = "xmobar"
myScratchTerminal = "urxvt"

------------------------------------------------------------------------
-- ScratchPad config
-- W.RationalRect l t w h
-- h : terminal height
-- w : terminal width
-- t : distance from top edge
-- l : distance from left edge
myManageScratchPad :: ManageHook
myManageScratchPad = scratchpadManageHook (W.RationalRect 0.5 0 0.5 0.4)


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch scratchpad
    , ((modm,               xK_s     ), scratchpadSpawnActionTerminal myScratchTerminal)

    -- launch dmenu
    , ((modm,               xK_x     ), spawn myDmenuLaunch)

    -- close focused window
    , ((modm .|. shiftMask, xK_x     ), spawn "xscreensaver-command -lock")

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

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- audio controls, require pulseaudio and pulseaudio-ctl
    , ((modm               , xF86XK_AudioMute), spawn "pulseaudio-ctl mute")
    , ((modm               , xF86XK_AudioLowerVolume), spawn "pulseaudio-ctl down")
    , ((modm               , xF86XK_AudioRaiseVolume), spawn "pulseaudio-ctl up")
    --, (xF86XK_AudioMicMute, spawn "pulseaudio-ctl mute-input")
    

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
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

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $
	onWorkspace "1:term" ( term ||| hterm ||| mytab ) $
	onWorkspaces [ "2:web", "5:pdf", "4:im" ] ( term ||| mytab ||| Full ) $
	term ||| Full ||| mytab
  where
     term = XMonad.Tall  1 (3/100) (1/2)
     hterm = Mirror term
     -- default tiling algorithm partitions the screen into two panes
     -- hterm  = Mirror term
     -- term   = HintedTile master delta inc TopLeft Tall
     -- master = 1
     -- delta  = 3/100
     ---inc    = 1/2
     mytab  = tabbed shrinkText myTabTheme


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = ( composeAll . concat $
    [ [ className =? "MPlayer"        --> doFloat        ]
    , [ className =? "Gimp"           --> doFloat        ]
    , [ resource  =? "desktop_window" --> doIgnore       ]
    , [ resource  =? "kdesktop"       --> doIgnore       ]
    , [ appName =? "web-edit"         --> doShift "2:web"]
    , [ fmap ( c `isInfixOf`) title --> doShift "3:mail" | c <- myMatchMailT ]
    , [ fmap ( c `isInfixOf`) title --> doShift "8:cal" | c <- myMatchCalT ]
    , [ fmap ( c `isInfixOf`) title --> doShift "6:org" | c <- myMatchOrgT ]
    , [ fmap ( c `isInfixOf`) className --> doShift "1:term" | c <- myMatchTermC ]
    , [ fmap ( c `isInfixOf`) className --> doShift "2:web" | c <- myMatchWebC ]
    , [ fmap ( c `isInfixOf`) className --> doShift "4:im" | c <- myMatchIMC ]
    , [ fmap ( c `isInfixOf`) title --> doShift "4:im" | c <- myMatchIMT ]
    , [ fmap ( c `isInfixOf`) className --> doShift "5:pdf" | c <- myMatchPDFC ]
    , [ fmap ( c `isInfixOf`) className --> doShift "9:wine" | c <- myMatchWineC ]
    -- make new windows slaves
    , [ doF avoidMaster ]
    ]) <+> manageDocks <+> myManageScratchPad
    where myMatchWebC = [ "Uzbl", "uzbl", "firefox", "Firefox", "Navigator", "web" , "luakit", "jumanji", "Google-chrome", "Chromium" ]
          myMatchTermC = [ "Terminator", "terminator", "urxvt", "URxvt", "xterm", "gnome-terminal" ]
          myMatchIMC = [ "Pidgin", "pidgin" , "irssi" ]
          myMatchIMT = [ "Pidgin", "pidgin" , "irssi", "viber", "Viber" ]
	  myMatchMailT = [ "mutt" ]
	  myMatchCalT = [ "cal" ]
	  myMatchOrgT = [ "org" ]
	  myMatchPDFC = [ "evince", "Evince", "acrobat", "xpdf", "Xpdf", "Zathura", "zathura", "display" ]
	  myMatchWineC = ["wine", "Wine", "explorer.exe", "spotify.exe", "winecfg.exe", "spotify", "Spotify" ]


avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c


------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = mempty <+> docksEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
wstitle :: String -> String
wstitle x = case x of
			"Mirror Tall"		-> "[-]"
			"Tall" 			-> "[|]"
			"Tabbed Simplest"   	-> "^^^"
		 	_			-> pad x


noScratchPad ws = if ws == "NSP" then "" else ws

myXmobarPP h = xmobarPP {
	ppOutput	= hPutStrLn h
	, ppTitle	= shorten 30
	, ppLayout	= xmobarColor "white" "black" . wstitle
	, ppUrgent	= xmobarColor "red" "black"
	, ppHidden	= noScratchPad
}

myLogHook h = dynamicLogWithPP $ myXmobarPP h

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = do
	spawn myTray

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
	myXmobarProc <- spawnPipe myXmobarLaunch
	xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook $ myXmobarProc,
        startupHook        = myStartupHook
    }
