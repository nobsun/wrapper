{-# LANGUAGE ImplicitParams #-}
module Main where

import ReplInfo
import Wrapper

main :: IO ()
main = let
    { ?cmdLine     = cmdLine defaultReplInfo
    ; ?prompt      = prompt defaultReplInfo
    ; ?isQuitCmd   = isQuitCmd defaultReplInfo
    ; ?cmdModifier = cmdModifier defaultReplInfo
    ; ?logSpec     = logSpec defaultReplInfo
    } in wrapper