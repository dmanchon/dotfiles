import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Wallpaper
import XMonad.Actions.GridSelect

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    ]

main = do
    xmproc <- spawnPipe "xmobar -x 1"
    setRandomWallpaper ["$HOME/Pictures/"]
    xmonad $ docks desktopConfig
        { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook defaultConfig
        , borderWidth = 2
        , terminal    = "urxvt"
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask, xK_a), spawn "pavucontrol")
        , ((mod4Mask, xK_f), spawn "nautilus")
        , ((mod4Mask, xK_x), spawn "emacsclient -c")
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        , ((mod4Mask, xK_z), spawn "xscreensaver-command -lock")
        ]