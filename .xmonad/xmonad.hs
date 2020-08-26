import           System.IO                 ( hPutStrLn )
import qualified XMonad                   as X
import           XMonad.Config.Desktop     ( desktopConfig )
import qualified XMonad.Hooks.DynamicLog  as Log
import qualified XMonad.Hooks.ManageDocks as Docks
import qualified XMonad.Layout.Spacing    as Space
import           XMonad.Util.Run           ( spawnPipe )
import           XMonad.Util.SpawnOnce     ( spawnOnce )

background = "#282a36"
darkGrey   = "#44475a"
grey       = "#6272a4"
white      = "#f8f8f2"
black1     = "#22232d"
black2     = "#1e2029"

cyan     = "#8be9fd"
green    = "#50fa7b"
orange   = "#ffb86c"
pink     = "#ff79c6"
purple   = "#bd93f9"
red      = "#ff5555"


layoutHook' =
  Docks.avoidStruts
  $ Space.spacingRaw True border True border True
  $ X.layoutHook X.def
  where border = Space.Border sp sp sp sp
        sp     = 10

logHook' xmproc = Log.dynamicLogWithPP Log.xmobarPP
  { Log.ppOutput          = hPutStrLn xmproc
  , Log.ppCurrent         = Log.xmobarColor green ""
  , Log.ppHidden          = Log.xmobarColor grey  ""
  , Log.ppTitle           = Log.xmobarColor white "" . Log.shorten 30
  , Log.ppSep             = " : "
  }

startupHook' = do
  spawnOnce "sleep 1 && feh --bg-scale ~/.config/bg.jpg &"
  spawnOnce "compton &"

main = do
  xmproc <- spawnPipe "pkill xmobar; xmobar  ~/.config/xmobar/config.hs"
  X.xmonad $ Docks.docks desktopConfig
    { X.borderWidth        = 3
    , X.focusedBorderColor = purple
    , X.layoutHook         = layoutHook'
    , X.logHook            = logHook' xmproc
    , X.modMask            = X.mod4Mask
    , X.normalBorderColor  = grey
    , X.startupHook        = startupHook'
    , X.terminal           = "kitty"
    }
