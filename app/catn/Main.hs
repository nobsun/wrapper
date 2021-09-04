{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment
import Text.Printf

main :: IO ()
main = do
    { args <- getArgs
    ; case args of
        []   -> return ()
        fp:_ -> putStr . numbering =<< readFile fp
    }

numbering :: String -> String
numbering = unlines . zipWith (printf "%4d  %s") [1::Int ..] . lines