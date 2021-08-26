import           XMonad                         ( (<+>)
                                                , Default(def)
                                                , XConf(normalBorder)
                                                , XConfig
                                                  ( borderWidth
                                                  , focusedBorderColor
                                                  , handleEventHook
                                                  , layoutHook
                                                  , logHook
                                                  , modMask
                                                  , normalBorderColor
                                                  , terminal
                                                  )
                                                , mod4Mask
                                                , xmonad
                                                )
import           XMonad.Core                    ( spawn )

import           XMonad.Hooks.DynamicLog        ( PP(ppOutput)
                                                , dynamicLogWithPP
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , ewmhDesktopsEventHook
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                , docksEventHook
                                                )

import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.Run                ( hPutStrLn
                                                , spawnPipe
                                                )

import           XMonad.Layout.Spacing          ( Border(Border)
                                                , spacingRaw
                                                )

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . docks . ewmh $ myConfig xmproc

myXmobarPP :: PP
myXmobarPP = def

myLayout =
  avoidStruts
    . spacingRaw True (Border 16 16 16 16) True (Border 8 8 8 8) True
    $ layoutHook def

myConfig xmproc =
  def
      { terminal           = "kitty"
      , modMask            = mod4Mask
      , borderWidth        = 3
      , logHook            = xmobarLogHook
      , layoutHook         = myLayout
      , handleEventHook    = docksEventHook
                             <+> fullscreenEventHook
                             <+> ewmhDesktopsEventHook
      , normalBorderColor  = "#ffffff"
      , focusedBorderColor = "#aa0aff"
      }
    `additionalKeysP` [ ("M-p", spawn "rofi -show drun")
                      , ("M-l", spawn "xscreensaver-command -lock")
                      , ("M-S-s", spawn "xfce4-screenshooter")
                      , ("M-]", spawn "chromium")
                      ]
 where
  xmobarLogHook = dynamicLogWithPP myXmobarPP { ppOutput = hPutStrLn xmproc }
