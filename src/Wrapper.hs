{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module Wrapper
    ( wrapper
    ) where

import Data.Time 
import Pipes
import System.Console.Haskeline
import System.IO
import System.Log.FastLogger
import System.Process

import ReplInfo

wrapper :: ( ?cmdLine     :: String
           , ?prompt      :: String
           , ?isQuitCmd   :: String -> Bool
           , ?cmdModifier :: CmdModifier
           , ?logSpec     :: LogType )
        => IO ()
wrapper = do
    { (proc, handles) <- invokeReplProcess ?cmdLine
    ; (logger, cleanupLogger) <- newFastLogger ?logSpec
    ; let { ?fromRepl = fst handles; ?toRepl = snd handles; ?logger = logger } in
        runInputT defaultSettings (runEffect wrapperLoop)
    ; msg <-hGetLine (fst handles)
    ; putStrLn msg
    ; logging logger msg
    ; cleanupLogger
    ; cleanupProcess proc
    }

type ProcessInfo = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

invokeReplProcess :: String -> IO (ProcessInfo, (Handle, Handle))
invokeReplProcess cmd = do
    { (procHandles, replHandles) <- initialPipeHandles
    ; proc <- createProcess (makeCreateProcess cmd replHandles)
    ; return (proc, procHandles)
    }

initialPipeHandles :: IO ((Handle, Handle), (Handle, Handle))
initialPipeHandles = do
    { (recvByRepl, sendToRepl)   <- createPipe
    ; (recvFromRepl, sendByRepl) <- createPipe
    ; mapM_ (`hSetBuffering` NoBuffering) [stdout, recvByRepl, sendToRepl, recvFromRepl, sendByRepl]
    ; mapM_ (`hSetEncoding` utf8) [recvByRepl, sendToRepl, recvFromRepl, sendByRepl]
    ; return ((recvFromRepl, sendToRepl), (recvByRepl, sendByRepl))
    }

makeCreateProcess :: String -> (Handle, Handle) -> CreateProcess
makeCreateProcess cmd (rh, sh)
    = (shell cmd) { std_in = UseHandle rh
                  , std_out = UseHandle sh
                  , std_err = UseHandle sh
                  }

wrapperLoop :: ( ?prompt      :: String
               , ?fromRepl    :: Handle
               , ?toRepl      :: Handle
               , ?isQuitCmd   :: String -> Bool
               , ?cmdModifier :: CmdModifier
               , ?logger      :: FastLogger )
            => Effect (InputT IO) ()
wrapperLoop = getResponse ?fromRepl >-> inputLn >-> takeUntil ?isQuitCmd >-> issue ?toRepl

getResponse :: ( ?prompt :: String
               , ?logger :: FastLogger )
            => Handle -> Producer () (InputT IO) ()
getResponse h = do 
    { resp <- liftIO $ hGetUntilNextPrompt h ?prompt
    ; let resp' = zipWith const resp (drop (1 + length ?prompt) resp)
    ; liftIO $ if null resp' then return () else putStrLn resp'
    ; liftIO $ logging ?logger resp'
    ; yield ()
    ; getResponse h
    }

inputLn :: ( ?prompt :: String
           , ?cmdModifier :: CmdModifier
           , ?logger :: FastLogger )
        => Pipe () String (InputT IO) ()
inputLn = do
    { () <- await
    ; inputLn'
    }

inputLn' :: ( ?prompt :: String
            , ?cmdModifier :: CmdModifier
            , ?logger :: FastLogger )
         => Pipe () String (InputT IO) ()
inputLn' = do
    { minput <- lift (getInputLine ?prompt)
    ; case minput of
        Nothing  -> return ()
        Just ""  -> inputLn'
        Just str -> do
            { iter (?cmdModifier str)
            ; inputLn
            }
    }
    where
        iter = \ case
            [s]  -> ylog s
            s:ss -> ylog s >> await >> iter ss
        ylog s = liftIO (logging ?logger (?prompt ++ s)) >> yield s

logging :: FastLogger -> String -> IO ()
logging logger str = do
    { logger (toLogStrLn str)
    ; logger . toLogStrLn . time =<< timeStamp 
    }
    where
        toLogStrLn = toLogStr . (++ "\n")
        time s = "<time>" ++ s ++ "</time>"

timeStamp :: IO String
timeStamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
    <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

takeUntil :: Functor m => (a -> Bool) -> Pipe a a m ()
takeUntil p = go
    where
        go = do
            { a <- await
            ; if p a
              then do
                  { yield a
                  ; return ()
                  }
              else do
                  { yield a
                  ; go
                  }
            }

hGetUntil :: Handle -> String -> IO String
hGetUntil h str = do
    { eof <- hIsEOF h
    ; if eof then return ""
      else do
          { c <- hGetChar h
          ; if c == head str then (c :) <$> getStr (tail str)
            else (c :) <$> hGetUntil h str
          }
    }
    where
        getStr []     = return ""
        getStr (c:cs) = do
            { eof <- hIsEOF h
            ; if eof then return ""
              else do
                  { c' <- hGetChar h
                  ; if c == c' then (c' :) <$> getStr cs
                    else (c' :) <$> hGetUntil h str
                  }
            }

hGetUntilNextPrompt :: Handle -> String -> IO String
hGetUntilNextPrompt h str = do
    { eof <- hIsEOF h
    ; if eof then return ""
      else do
          { c <- hGetChar h
          ; if c == head str then (c :) <$> getStr (tail str)
            else (c :) <$> hGetUntil h ('\n' : str)
          }
    }
    where
        getStr []     = return ""
        getStr (c:cs) = do
            { eof <- hIsEOF h
            ; if eof then return ""
              else do
                  { c' <- hGetChar h
                  ; if c == c' then (c' :) <$> getStr cs
                    else (c' :) <$> hGetUntil h ('\n' : str)
                  }
            }

issue :: Handle -> Consumer String (InputT IO) ()
issue h = do
    { liftIO . hPutStrLn h =<< await
    ; issue h
    }