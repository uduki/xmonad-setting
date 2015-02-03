import           Control.Exception
import           Control.Monad
import qualified Data.Map                            as M
import           Data.Ratio                          ((%))
--import DBus
--import DBus.Connection
--import DBus.Message
import           XMonad                              hiding ((|||))
import           XMonad.Actions.TagWindows
import           XMonad.Actions.UpdatePointer
--import XMonad.Config.Gnome
import           XMonad.Config.Kde
--import XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Combo
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators     ((|||))
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed
import           XMonad.ManageHook
import           XMonad.Prompt
import qualified XMonad.StackSet                     as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Scratchpad

myConfig = kde4Config

myLayout = avoidStruts $ smartBorders (mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) layouts) ||| staticLayouts
    where
    layouts = rTall ||| Grid ||| Full ||| skype
    myResizableTall = combineTwo rTall tab tab
    staticLayouts = tab ||| gimp
    tab = tabbed shrinkText defaultTheme
    rTall = ResizableTall 1 (3/100) (1/2) []
    skype = IM (1%7) (ClassName "Skype")
    gimp = withIM 0.11 (Role "gimp-toolbox") $ reflectHoriz $ withIM 0.15 (Role "gimp-dock") tab

myManage = composeAll . concat $ [ [(role =? "gimp-dock" <||> role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)]
                                 , [className =? c --> doFloat | c <- myFloats]
                                 , [title =? t --> doFloat | t <- myOtherFloats]
                                 , [resource =? r --> doIgnore | r <- myIgnoreResources]
                                 ]
    where
    myFloats = ["MPlayer", "Plasma-desktop", "Plasma", "kmix", "Lancelot", "Klipper"]
    myOtherFloats = ["alsamixer", "Plasma-desktop", "kmix", "Input Helper"]
    myIgnoreResources = ["Do", "Lancelot"]
    role = stringProperty "WM_WINDOW_ROLE"

myKey x = [ ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
          , ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
          , ((modm, xK_a), tagPrompt defaultXPConfig (withFocused . addTag))
          , ((modm, xK_d), tagDelPrompt defaultXPConfig)
          , ((modm, xK_s), tagPrompt defaultXPConfig focusUpTaggedGlobal)
          , ((modm, xK_x), sendMessage $ Toggle REFLECTX)
          , ((modm, xK_y), sendMessage $ Toggle REFLECTY)
          , ((modm, xK_o), namedScratchpadAction myScratchpads "Synapse")
          , ((modm, xK_Print), namedScratchpadAction myScratchpads "PrintScreen")
          , ((modm .|. shiftMask, xK_m), sendMessage $ Toggle MIRROR)
          , ((modm, xK_f), updatePointer (Relative 0.5 0.5))
          , ((modm .|. shiftMask, xK_t), scratchpadSpawnActionCustom "transset -p .92")
          --, ((modm .|. shiftMask, xK_Print), namedScratchpadAction myScratchpads "PrintScreenAtWindow")
          ]
    where
    modm = modMask x

myScratchpads :: NamedScratchpads
myScratchpads = [ --NS "PrintScreen" "gnome-panel-screenshot" (className =? "gnome-panel-screenshot") defaultFloating
                  NS "PrintScreen" "ksnapshot" (className =? "ksnapshot") defaultFloating
                --, NS "GnomeDo" "gnome-do" (className =? "gnome-do") defaultFloating
                --, NS "PrintScreenAtWindow" "gnome-screenshot -window" (className =? "gnome-screenshot") defaultFloating
                , NS "Synapse" "synapse" (title =? "synapse") defaultFloating
                ]

main :: IO()
main =
    xmonad $ myConfig {
        startupHook = startupHook myConfig >> setWMName "LG3D"
        , terminal = "urxvtc -e tmux"
        , layoutHook = myLayout
        , manageHook = myManage <+> manageHook myConfig
        , modMask = mod3Mask
        , keys = \ x -> keys myConfig x `M.union` M.fromList (myKey x)
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#cc0000"
    }


{-
 - OLD SETTING.
 -}
--getWellKnownName :: Connection -> IO()
--getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) -> getWellKnownName dbus)
--    where
--        tryGetName = do
--            namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
--            addArgs namereq [String "org.xmonad.Log", Word32 5]
--            sendWithReplyAndBlock dbus namereq 0
--            return ()
--
--
--main :: IO()
--main = withConnection Session $ \ dbus -> do
--    putStrLn "Getting well-known name."
--    getWellKnownName dbus
--    putStrLn "Got name, starting XMonad."
--    xmonad $ myConfig {
--        startupHook = startupHook myConfig >> setWMName "LG3D"
--        , logHook = do
--            logHook myConfig
--            dynamicLogWithPP $ defaultPP{
--                ppOutput = \ str -> do
--                    let str' = "<span font=\"Takao P Gothic\">" ++ str ++ "</span>"
--                    msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
--                    addArgs msg [String str']
--                    -- if the send fails, ignore it.
--                    send dbus msg 0 `catchDyn` (\ (DBus.Error _name _msg) -> return 0)
--                    return ()
--                , ppTitle = pangoColor "#dd7070" . shorten 60 . escape
--                , ppCurrent = pangoColor "#dd7070" . wrap "[" "]"
--                , ppVisible = pangoColor "#44cccc" . wrap "_" ""
--                , ppHidden = wrap "" ""
--                , ppUrgent = pangoColor "red"
--                }
--        , layoutHook = myLayout
--        , manageHook = myManage
--        , modMask = mod3Mask
--        , keys = \ x -> M.union (keys myConfig x) (M.fromList (myKey x))
--        , normalBorderColor = "#004402"
--        , focusedBorderColor = "#ff8b91"
--    }
--
--pangoColor :: String -> String -> String
--pangoColor fg = wrap left right
--    where
--        left = "<span foreground=\"" ++ fg ++ "\">"
--        right = "</span>"
--
--escape :: String -> String
--escape = concatMap escapeChar
--escapeChar :: Char -> String
--escapeChar '<' = "&lt;"
--escapeChar '>' = "&gt;"
--escapeChar '&' = "&amp;"
--escapeChar '"' = "&quot;"
--escapeChar c = [c]
