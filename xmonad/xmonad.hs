import           System.IO                         ( hPutStrLn )
import           XMonad                            ( (.|.) )
import qualified XMonad                           as X
import qualified XMonad.Actions.Navigation2D      as Nav
import qualified XMonad.Actions.WorkspaceNames    as Names
import           XMonad.Config.Desktop             ( desktopConfig )
import qualified XMonad.Hooks.DynamicLog          as Log
import qualified XMonad.Hooks.ManageDocks         as Docks
import qualified XMonad.Layout.IndependentScreens as IS
import qualified XMonad.Layout.Spacing            as Space
import           XMonad.Util.Run                   ( spawnPipe )
import           XMonad.Util.SpawnOnce             ( spawnOnce )
import           XMonad.Util.EZConfig              ( additionalKeys )

background = "#282a36"
darkGrey   = "#44475a"
grey       = "#6272a4"
white      = "#f8f8f2"
black1     = "#22232d"
black2     = "#1e2029"

cyan   = "#8be9fd"
green  = "#50fa7b"
orange = "#ffb86c"
pink   = "#ff79c6"
purple = "#bd93f9"
red    = "#ff5555"

-- | Leader key.
modMask = X.mod4Mask

-- | Default applications.
browser  = "firefox"
editor   = "emacs"
terminal = "kitty"

pp xmproc = Log.xmobarPP
  { Log.ppOutput          = hPutStrLn xmproc
  , Log.ppCurrent         = Log.xmobarColor green ""
  , Log.ppHidden          = Log.xmobarColor grey  ""
  , Log.ppTitle           = const ""
  , Log.ppSep             = "  "
  }

main = do
  n <- IS.countScreens
  xmprocs <- mapM spawnBar [0 .. n - 1]
  X.xmonad $ Nav.withNavigation2DConfig X.def $ Docks.docks desktopConfig
    { X.borderWidth        = 5
    , X.focusedBorderColor = purple
    , X.layoutHook         = layoutHook'
    , X.logHook            = logHook' xmprocs
    , X.modMask            = modMask
    , X.normalBorderColor  = grey
    , X.startupHook        = startupHook'
    , X.terminal           = terminal
    }
    `additionalKeys`
    [
    -- Application shortcuts.
      ((modMask,                 X.xK_w    ), X.spawn browser               )
    , ((modMask,                 X.xK_e    ), X.spawn editor                )
    , ((modMask,                 X.xK_t    ), X.spawn terminal              )
    -- Close window.
    , ((modMask,                 X.xK_q    ), X.kill                        )
    -- Reload config.
    , ((modMask,                 X.xK_r    ), resetWM                       )
    -- Rename current workspace.
    , ((modMask,                 X.xK_n    ), Names.renameWorkspace X.def   )
    -- Switch between layers                                                )
    , ((modMask .|. X.shiftMask, X.xK_space), Nav.switchLayer               )
    -- Directional navigation of windows                                    )
    , ((modMask,                 X.xK_h    ), Nav.windowGo Nav.L False      )
    , ((modMask,                 X.xK_j    ), Nav.windowGo Nav.D False      )
    , ((modMask,                 X.xK_k    ), Nav.windowGo Nav.U False      )
    , ((modMask,                 X.xK_l    ), Nav.windowGo Nav.R False      )
    -- Swap adjacent windows               )                                )
    , ((modMask .|. X.shiftMask, X.xK_h    ), Nav.windowSwap Nav.L False    )
    , ((modMask .|. X.shiftMask, X.xK_j    ), Nav.windowSwap Nav.D False    )
    , ((modMask .|. X.shiftMask, X.xK_k    ), Nav.windowSwap Nav.U False    )
    , ((modMask .|. X.shiftMask, X.xK_l    ), Nav.windowSwap Nav.R False    )
    -- Directional navigation of screens                                    )
    , ((modMask,                 X.xK_a    ), Nav.screenGo Nav.L False      )
    , ((modMask,                 X.xK_s    ), Nav.screenGo Nav.D False      )
    , ((modMask,                 X.xK_d    ), Nav.screenGo Nav.U False      )
    , ((modMask,                 X.xK_f    ), Nav.screenGo Nav.R False      )
    -- Swap workspaces on adjacent screens                                  )
    , ((modMask .|. X.shiftMask, X.xK_a    ), Nav.screenSwap Nav.L False    )
    , ((modMask .|. X.shiftMask, X.xK_s    ), Nav.screenSwap Nav.D False    )
    , ((modMask .|. X.shiftMask, X.xK_d    ), Nav.screenSwap Nav.U False    )
    , ((modMask .|. X.shiftMask, X.xK_f    ), Nav.screenSwap Nav.R False    )
    -- Send window to adjacent screen
    , ((modMask .|. X.mod1Mask,  X.xK_a    ), Nav.windowToScreen Nav.L False)
    , ((modMask .|. X.mod1Mask,  X.xK_s    ), Nav.windowToScreen Nav.D False)
    , ((modMask .|. X.mod1Mask,  X.xK_d    ), Nav.windowToScreen Nav.U False)
    , ((modMask .|. X.mod1Mask,  X.xK_f    ), Nav.windowToScreen Nav.R False)
    -- Shrink the master area
    -- , ((modMask,                 X.xK_<    ), X.sendMessage X.Shrink        )
    -- Expand the master area
    -- , ((modMask,                 X.xK_>    ), X.sendMessage X.Expand        )
    ]

resetWM = X.spawn "xmonad --recompile; xmonad --restart"

spawnBar screenId = spawnPipe $
  "xmobar -x " ++ show screenId ++ " /home/jeremy/.config/xmobar/xmobar.hs"

killAllBars = spawnOnce "pkill xmobar"

layoutHook' =
  Docks.avoidStruts
  -- $ Space.spacingRaw True border True border True
  $ X.layoutHook X.def
  where border = Space.Border sp sp sp sp
        sp     = 7

logHook' xmprocs =
  mapM_ (\x -> Names.workspaceNamesPP (pp x) >>= Log.dynamicLogWithPP) xmprocs

startupHook' = spawnOnce
    "sleep 1 && feh --bg-scale /home/jeremy/.config/bg.jpg"
