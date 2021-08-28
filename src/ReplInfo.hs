module ReplInfo where

import System.Log.FastLogger

type CmdModifier = String -> [String]

data ReplInfo
    = ReplInfo
    { cmdLine   :: String
    , prompt    :: String
    , isQuitCmd :: String -> Bool
    , cmdModifier :: CmdModifier
    , logSpec     :: LogType
    }
