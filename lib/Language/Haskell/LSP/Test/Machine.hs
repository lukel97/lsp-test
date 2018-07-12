module Language.Haskell.LSP.Test.Machine where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Test

data State = State String (FromServerMessage -> Bool) [Session ()] State
           | Passed
           | Failed

data Event = TimeoutEvent | Received FromServerMessage

advance :: State -> Event -> Session State
advance _ TimeoutEvent = return Failed
advance s@(State name f actions next) (Received msg)
  | f msg = do
    liftIO $ putStrLn name
    sequence_ actions
    return next
  | otherwise = return s
advance s _ = return s

mkStates [] = Passed
mkStates ((n, f, msgs):xs) = State n f msgs (mkStates xs)

runMachine :: String -> FilePath -> [(String, FromServerMessage -> Bool, [Session ()])] -> IO Bool
runMachine cmd rootDir encodedStates =
  runSession cmd rootDir $ do
    let f Passed = return Passed
        f s = Received <$> anyMessage >>= advance s >>= f
        initState = mkStates encodedStates
    res <- f initState
    case res of
      Passed -> return True
      _ -> return False

