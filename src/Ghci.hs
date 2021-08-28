module Ghci 
    ( ghciInfo
    ) where

import System.Log.FastLogger
import System.Process

import ReplInfo

ghciInfo :: ReplInfo
ghciInfo = ReplInfo
    { prompt = ">>> "
    , cmdLine = "stack exec -- ghci -fshow-loaded-modules"
    , isQuitCmd = (`elem` [":q", ":quit"])
    , cmdModifier = \ str -> case words str of
        [cmd]     | cmd `elem` [":r", ":reload"] -> [":e"]
        [cmd, fp] | cmd `elem` [":l", ":load"]   -> [":e " ++ fp, str]
        _                                        -> [str]
    , logSpec = LogFile (FileLogSpec "ghci.log" (2^(26 :: Int)) 10) (defaultBufSize * 16)
    }
