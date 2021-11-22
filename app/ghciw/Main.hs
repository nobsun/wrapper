{-# LANGUAGE ImplicitParams #-}
module Main where

import System.Environment
import ReplInfo
import Ghci
import Wrapper

main :: IO ()
main = do
    { args <- getArgs
    ; let
        { ?cmdLine     = cmdLine ghciInfo ++ " "
                      ++ unwords args ++ " "
                      ++ "2>&1"
        ; ?prompt      = prompt ghciInfo
        ; ?isQuitCmd   = isQuitCmd ghciInfo
        ; ?cmdModifier = cmdModifier ghciInfo
        ; ?logSpec     = logSpec ghciInfo
        } in wrapper
    }