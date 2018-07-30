module Language.Haskell.LSP.Test.Server (withServer) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test.Compat
import System.IO
import System.Process

withServer :: MonadIO m => String -> Bool -> (Handle -> Handle -> Int -> m a) -> m a
withServer serverExe logStdErr f = do
  -- TODO Probably should just change runServer to accept
  -- separate command and arguments
  let cmd:args = words serverExe
      createProc = (proc cmd args) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just serverIn, Just serverOut, Just serverErr, serverProc) <- liftIO $ createProcess createProc

  -- Need to continuously consume to stderr else it gets blocked
  -- Can't pass NoStream either to std_err
  liftIO $ hSetBuffering serverErr NoBuffering
  errSinkThread <- liftIO $ forkIO $ forever $ hGetLine serverErr >>= when logStdErr . putStrLn

  pid <- liftIO $ getProcessID serverProc

  result <- f serverIn serverOut pid

  liftIO $ killThread errSinkThread
  liftIO $ terminateProcess serverProc
  return result
