import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import System.Exit

main = do
    xmonad $
        defaultConfig {
            terminal = "xterm"
        } `additionalKeysP` [
            ("M-S-w", io $ exitWith ExitSuccess)
        ]
