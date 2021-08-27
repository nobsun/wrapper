module Ghci 
    ( prompt
    , cmdLine
    , isQuitCmd
    , cmdModifier
    , logSpec
    ) where

import System.Log.FastLogger
import System.Process

prompt :: String
prompt = ">>> "

cmdLine :: String
cmdLine = "stack exec -- ghci"

isQuitCmd :: String -> Bool
isQuitCmd = (`elem` [":q", ":quit"])

cmdModifier :: String -> String
cmdModifier str = case words str of
    [cmd]     | cmd `elem` [":r", ":reload"] -> ":e"
    _                                        -> str

logFile :: FilePath
logFile = "ghci.log"

logSpec :: LogType
logSpec = LogFile (FileLogSpec "ghci.log" (2^(26 :: Int)) 10) (defaultBufSize * 16)