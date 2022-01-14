{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module GhciInput
    ( ghciinput
    ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char
import System.Console.Haskeline
import System.IO

ghciinput :: IO ()
ghciinput = runInputT defaultSettings
          $ withInterrupt loop
   where
       loop :: InputT IO ()
       loop = handle (\ Interrupt -> loop) $ do
           minput <- getInputLine ""
           case minput of
               Nothing -> return ()
               Just input
                    | input `elem` [":q",":quit"]
                        -> liftIO (putStrLn input) >> return ()
                    | otherwise
                        -> case words input of
                            w : ws
                                | w `elem` [":r", ":reload"]
                                    -> liftIO (putStrLn ":e") >> loop
                                | w `elem` [":l", ":load"]
                                    -> liftIO (putStrLn (unwords (":e" : ws)))
                                       >> outputStrLn input
                                       >> liftIO (putStrLn input)
                                       >> loop
                            _       -> liftIO (putStrLn input) >> loop
