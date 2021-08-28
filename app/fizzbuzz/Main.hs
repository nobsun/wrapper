{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.IO.Class
import Data.Bool
import Data.Char
import System.Console.Haskeline
import qualified ReplInfo as RI

prompt :: String
prompt = RI.prompt RI.defaultReplInfo

isQuitCmd :: String -> Bool
isQuitCmd = RI.isQuitCmd RI.defaultReplInfo

main :: IO ()
main = do
    { putStrLn "Welcome to FizzBuzz! Enjoy!"
    ; runInputT defaultSettings loop
    }
    where
        loop = do
            { minput <- getInputLine prompt
            ; case minput of
                Nothing  -> return ()
                Just ""  -> loop
                Just str
                    | isQuitCmd str -> liftIO (putStrLn "Bye bye! Happy FizzBuzzing!")
                    | otherwise     -> liftIO (putStrLn (fizzbuzz str)) >> loop
            }

genFizzBuzz :: (String, Int) -> Int -> String
genFizzBuzz (s,p) n = bool "" s (n `mod` p == 0)

fizzbuzz :: String -> String
fizzbuzz = \ case
    str | not (all isDigit str)
        -> "NaN: " ++ str
        | otherwise
        -> concatMap (($ read str) . genFizzBuzz) [("Fizz",3), ("Buzz",5)] <+ str

(<+) :: [a] -> [a] -> [a]
[] <+ ys = ys
xs <+ _  = xs

infixr 4 <+
