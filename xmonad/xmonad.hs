import           System.IO                            ( hPutStrLn )
import           XMonad                               ( (.|.) )
import qualified XMonad                              as X
import qualified XMonad.Actions.Navigation2D         as Nav
import qualified XMonad.Actions.WorkspaceNames       as Names
import           XMonad.Config.Desktop                ( desktopConfig )
import qualified XMonad.Hooks.DynamicLog             as Log
import qualified XMonad.Hooks.EwmhDesktops           as EWMH
import qualified XMonad.Hooks.ManageDocks            as Docks
import qualified XMonad.Layout.BinarySpacePartition  as BSP
import           XMonad.Layout.Grid                   ( Grid(Grid) )
import qualified XMonad.Layout.IndependentScreens    as IS
import           XMonad.Layout.LayoutCombinators      ( (|||) )
import qualified XMonad.Layout.LayoutCombinators     as Layout
import           XMonad.Layout.NoBorders              ( smartBorders)
import qualified XMonad.Layout.Spacing               as Spacing
import           XMonad.Layout.ThreeColumns           ( ThreeCol(ThreeColMid) )
import qualified XMonad.StackSet                     as Stack
import           XMonad.Util.EZConfig                 ( additionalKeys )
import           XMonad.Util.Run                      ( spawnPipe )
import           XMonad.Util.SpawnOnce                ( spawnOnce )

-- | Leader key.
modMask = X.mod4Mask

borderWidth = 3 -- Size of window borders.
gap         = 0 -- Initial gap between windows (without border).
gapStep     = 5 -- Step size when incrementing/decrementing gap.

-- | Default applications.
browser  = "firefox-devedition"
privateBrowser = "firefox-devedition --private-window"
editor   = "emacs"
terminal = "kitty"

-- | Bright colors.
cyan   = "#8be9fd"
green  = "#50fa7b"
orange = "#ffb86c"
pink   = "#ff79c6"
purple = "#bd93f9"
red    = "#ff5555"

-- | Greyscale colors.
background = "#282a36"
darkGrey   = "#44475a"
grey       = "#6272a4"
white      = "#f8f8f2"
black1     = "#22232d"
black2     = "#1e2029"

-- | Border colors.
normalBorderColor  = background
focusedBorderColor = pink

-- | Workspace colours in top bar.
xmobarCurrentColor = green
xmobarHiddenColor  = grey

main = do
  n       <- IS.countScreens
  xmprocs <- mapM (spawnPipe . runXMobar) [0 .. n - 1]
  X.xmonad
    $ EWMH.ewmh
    $ Nav.withNavigation2DConfig X.def
    $ Docks.docks desktopConfig
    { X.borderWidth        = borderWidth
    , X.focusedBorderColor = focusedBorderColor
    , X.layoutHook         = layoutHook'
    , X.logHook            = logHook' xmprocs
    , X.modMask            = modMask
    , X.normalBorderColor  = normalBorderColor
    , X.terminal           = terminal
    , X.handleEventHook    = EWMH.fullscreenEventHook
    }
    `additionalKeys`
    ([
    -- Application shortcuts.
      ((modMask,                 X.xK_w            ), X.spawn browser            )
    , ((modMask .|. X.shiftMask, X.xK_w            ), X.spawn privateBrowser     )
    , ((modMask,                 X.xK_e            ), X.spawn editor             )
    , ((modMask,                 X.xK_r            ), X.spawn "rofi -show drun"  )
    , ((modMask .|. X.shiftMask, X.xK_r            ), X.spawn "rofi -show run"   )
    , ((modMask,                 X.xK_t            ), X.spawn terminal           )
    -- Application shortcuts.
    , ((modMask,                 X.xK_y            ), setLayout "Tall"           )
    , ((modMask,                 X.xK_u            ), setLayout "ThreeCol"       )
    , ((modMask,                 X.xK_i            ), setLayout "Grid"           )
    , ((modMask,                 X.xK_o            ), setLayout "BSP"            )
    , ((modMask,                 X.xK_p            ), setLayout "Full"           )
    -- Spacing.
    , ((modMask,                 X.xK_bracketright ), incSpacing                 )
    , ((modMask,                 X.xK_bracketleft  ), decSpacing                 )
    -- Close/lock.
    , ((modMask,                 X.xK_c            ), X.kill                     )
    , ((modMask,                 X.xK_q            ), X.spawn lockScreen         )
    -- Update XMonad.
    , ((modMask,                 X.xK_x            ), X.spawn updateWM           )
    -- Name current workspace.
    , ((modMask,                 X.xK_n            ), Names.renameWorkspace X.def)
    -- Directional navigation of windows.
    , ((modMask,                 X.xK_h            ), Nav.windowGo   Nav.L False )
    , ((modMask,                 X.xK_j            ), Nav.windowGo   Nav.D False )
    , ((modMask,                 X.xK_k            ), Nav.windowGo   Nav.U False )
    , ((modMask,                 X.xK_l            ), Nav.windowGo   Nav.R False )
    -- Swap adjacent windows.
    , ((modMask .|. X.shiftMask, X.xK_h            ), Nav.windowSwap Nav.L False )
    , ((modMask .|. X.shiftMask, X.xK_j            ), Nav.windowSwap Nav.D False )
    , ((modMask .|. X.shiftMask, X.xK_k            ), Nav.windowSwap Nav.U False )
    , ((modMask .|. X.shiftMask, X.xK_l            ), Nav.windowSwap Nav.R False )
    -- Directional navigation of screens.
    , ((modMask,                 X.xK_a            ), Nav.screenGo   Nav.L False )
    , ((modMask,                 X.xK_s            ), Nav.screenGo   Nav.D False )
    , ((modMask,                 X.xK_d            ), Nav.screenGo   Nav.U False )
    , ((modMask,                 X.xK_f            ), Nav.screenGo   Nav.R False )
    -- Move workspace to adjacent screen.
    , ((modMask .|. X.shiftMask, X.xK_a            ), Nav.screenSwap Nav.L False )
    , ((modMask .|. X.shiftMask, X.xK_s            ), Nav.screenSwap Nav.D False )
    , ((modMask .|. X.shiftMask, X.xK_d            ), Nav.screenSwap Nav.U False )
    , ((modMask .|. X.shiftMask, X.xK_f            ), Nav.screenSwap Nav.R False )
    -- Shrink and expand the main area.
    , ((modMask .|. X.shiftMask, X.xK_period       ), X.sendMessage X.Shrink     )
    , ((modMask .|. X.shiftMask, X.xK_comma        ), X.sendMessage X.Expand     )
    -- Switch from greedyView to view.
    ] ++ [
      ((modMask .|. mask,        xK                ), X.windows $ action i       )
      | (i, xK)        <- zip (map show [1 .. 9]) [X.xK_1 .. X.xK_9]
      , (action, mask) <- [(Stack.view, 0), (Stack.shift, X.shiftMask)]
    ])
    where setLayout = X.sendMessage . Layout.JumpToLayout

layoutHook' =
    smartBorders
  $ Docks.avoidStruts -- Layouts take top bar, the "dock", into account.
  $ Spacing.spacingRaw True border True border True
  $     X.Tall      1 (3/100) (1/2)
    ||| ThreeColMid 1 (3/100) (1/2)
    ||| Grid
    ||| BSP.emptyBSP
    ||| X.Full
  where border = Spacing.Border gap gap gap gap

-- | Commands.
updateWM         = "home-manager switch; xmonad --recompile; xmonad --restart"
lockScreen       = "betterlockscreen -l"
runXMobar screen = "xmobar -x " ++ show screen ++ " ~/.config/xmobar/xmobar.hs"
incSpacing       = Spacing.incWindowSpacing gapStep >> Spacing.incScreenSpacing gapStep
decSpacing       = Spacing.decWindowSpacing gapStep >> Spacing.decScreenSpacing gapStep

-- | Information to send to XMobar.
pp xmproc = Log.xmobarPP
  { Log.ppOutput          = hPutStrLn xmproc
  , Log.ppCurrent         = Log.xmobarColor xmobarCurrentColor ""
  , Log.ppHidden          = Log.xmobarColor xmobarHiddenColor  ""
  , Log.ppHiddenNoWindows = Log.xmobarColor xmobarHiddenColor  ""
  , Log.ppTitle           = const ""
  , Log.ppSep             = "  "
  }

logHook' xmprocs = flip mapM_ xmprocs $ \xmproc -> do
  workspaceNamesPP (pp xmproc) >>= Log.dynamicLogWithPP

-- * Override XMonad.Actions.WorkspaceNames.
-- TODO: factor out and upstream.

getWorkspaceNames :: X.X (X.WorkspaceId -> String)
getWorkspaceNames = do
  lookup <- Names.getWorkspaceNames'
  -- return $ \wks -> wks ++ maybe "" (':' :) (lookup wks)
  return $ \wks -> maybe wks id (lookup wks)

-- | A PP with modified workspace names.
workspaceNamesPP :: Log.PP -> X.X Log.PP
workspaceNamesPP pp' = do
  names <- getWorkspaceNames
  return $ pp'
    { Log.ppCurrent         = Log.ppCurrent         pp' . names
    , Log.ppVisible         = Log.ppVisible         pp' . names
    , Log.ppHidden          = Log.ppHidden          pp' . names
    , Log.ppHiddenNoWindows = Log.ppHiddenNoWindows pp' . names
    , Log.ppUrgent          = Log.ppUrgent          pp' . names
    }
