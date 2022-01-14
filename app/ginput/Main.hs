module Main where

import System.IO
import GhciInput

main :: IO ()
main = hSetBuffering stdout NoBuffering >> ghciinput