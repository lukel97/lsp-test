{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Machine where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Test

data State = State String (FromServerMessage -> Bool) [Session ()] State
           | Passed
           | Failed

data Event = Timeout | Received FromServerMessage

advance :: State -> Event -> Session State
advance _ Timeout = return Failed
advance s@(State name f actions next) (Received msg)
  | f msg = do
    liftIO $ putStrLn name
    sequence_ actions
    return next
  | otherwise = return s
advance s _ = return s

mkStates [] = Passed
mkStates ((n, f, msgs):xs) = State n f msgs (mkStates xs)

runMachine :: String -> [(String, FromServerMessage -> Bool, [Session ()])] -> IO String
runMachine rootDir encodedStates =
  runSession "hie --lsp" rootDir $ do
    let f Passed = return Passed
        f s = Received <$> anyMessage >>= advance s >>= f
        initState = mkStates encodedStates
    res <- f initState
    case res of
      Passed -> return "passed"
      _ -> return "failed"

