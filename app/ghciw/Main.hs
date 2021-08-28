{-# LANGUAGE ImplicitParams #-}
module Main where

import System.Environment
import Ghci
import Wrapper

main :: IO ()
main = do
    { args <- getArgs
    ; let
        { ?cmdLine     = cmdLine ++ " " ++ unwords args
        ; ?prompt      = prompt
        ; ?isQuitCmd   = isQuitCmd
        ; ?cmdModifier = cmdModifier
        ; ?logSpec     = logSpec
        } in wrapper
    }