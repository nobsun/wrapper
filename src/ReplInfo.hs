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

defaultReplInfo = ReplInfo
    { cmdLine = "fizzbuzz"
    , prompt  = "? "
    , isQuitCmd = ("bye" ==)
    , cmdModifier = (: [])
    , logSpec = LogFile (FileLogSpec "fizzbuzz.log" (2^(20 :: Int)) 10) 16
    }
