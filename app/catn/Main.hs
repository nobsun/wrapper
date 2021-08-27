{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Default
import System.Environment
import Text.Printf

instance {-# Overlapping #-} Default FilePath where
    def = "src/Lib.hs"

main :: IO ()
main = do
    { args <- getArgs
    ; let fp = case args of { [] -> def @FilePath; a:_ -> a }
    ; putStr . numbering =<< readFile fp
    }

numbering :: String -> String
numbering = unlines . zipWith (printf "%6d  %s") [1::Int ..] . lines