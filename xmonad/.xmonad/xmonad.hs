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

import           XMonad.Hooks.DynamicLog
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

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . docks . ewmh $ myConfig xmproc

toJapanese :: String -> String
toJapanese "1" = "一"
toJapanese "2" = "二"
toJapanese "3" = "三"
toJapanese "4" = "四"
toJapanese "5" = "五"
toJapanese "6" = "六"
toJapanese "7" = "七"
toJapanese "8" = "八"
toJapanese "9" = "九"
toJapanese x   = x

myXmobarPP :: PP
myXmobarPP = def { ppCurrent         = xmobarColor "white" "" . toJapanese
                 , ppVisible         = wrap "(" ")" . toJapanese
                 , ppHidden          = toJapanese
                 , ppHiddenNoWindows = const ""
                 , ppUrgent          = xmobarColor "red" "yellow" . toJapanese
                 , ppLayout          = const ""
                 , ppTitle           = xmobarColor "white" "" . shorten 80
                 , ppSep             = "  :  "
                 }

myLayout =
  avoidStruts
    . spacingRaw False (Border 16 16 16 16) True (Border 8 8 8 8) True
    $ layoutHook def

myConfig xmproc =
  def
      { terminal           = "alacritty"
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
    `additionalKeysP` [ ("M-p"  , spawn "rofi -show drun")
                      , ("M-l"  , spawn "loginctl lock-session")
                      , ("M-S-s", spawn "flameshot gui")
                      , ("M-]"  , spawn "chromium")
                      ]
 where
  xmobarLogHook = dynamicLogWithPP myXmobarPP { ppOutput = hPutStrLn xmproc }
