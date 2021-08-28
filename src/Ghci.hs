module Ghci 
    ( prompt
    , cmdLine
    , isQuitCmd
    , cmdModifier
    , logSpec
    ) where

import System.Log.FastLogger
import System.Process

type CmdModifier = String -> String

prompt :: String
prompt = ">>> "

cmdLine :: String
cmdLine = "stack exec -- ghci -fshow-loaded-modules"

isQuitCmd :: String -> Bool
isQuitCmd = (`elem` [":q", ":quit"])

cmdModifier :: CmdModifier
cmdModifier str = case words str of
    [cmd]     | cmd `elem` [":r", ":reload"] -> head [":e"]
    -- [cmd, fp] | cmd `elem` [":l", ":load"]   -> head [":e " ++ fp, str]
    _                                        -> head [str]

logFile :: FilePath
logFile = "ghci.log"

logSpec :: LogType
logSpec = LogFile (FileLogSpec "ghci.log" (2^(26 :: Int)) 10) (defaultBufSize * 16)
