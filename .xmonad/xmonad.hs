import           System.IO                 ( hPutStrLn )
import qualified XMonad                   as X
import           XMonad.Config.Desktop     ( desktopConfig )
import qualified XMonad.Hooks.DynamicLog  as Log
import qualified XMonad.Hooks.ManageDocks as Docks
import qualified XMonad.Layout.Spacing    as Space
import           XMonad.Util.Run           ( spawnPipe )
import           XMonad.Util.SpawnOnce     ( spawnOnce )

border = Space.Border 7 7 7 7

layoutHook' =
  Docks.avoidStruts
  $ Space.spacingRaw True border True border True
  $ X.layoutHook X.def

logHook' xmproc = Log.dynamicLogWithPP Log.xmobarPP
  { Log.ppOutput = hPutStrLn xmproc
  , Log.ppTitle  = Log.xmobarColor "green" "" . Log.shorten 50
  }

startupHook' = do
  spawnOnce "sleep 0.5 && feh --bg-scale ~/.config/bg.jpg &"
  spawnOnce "compton &"

main = do
  xmproc <- spawnPipe "xmobar"
  X.xmonad $ Docks.docks desktopConfig
    { X.borderWidth        = 3
    , X.focusedBorderColor = "#bd93f9"
    , X.layoutHook         = layoutHook'
    , X.logHook            = logHook' xmproc
    , X.modMask            = X.mod4Mask
    , X.normalBorderColor  = "#282a36"
    , X.startupHook        = startupHook'
    , X.terminal           = "kitty"
    }
