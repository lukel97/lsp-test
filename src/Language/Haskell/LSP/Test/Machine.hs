{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Machine where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Test.Messages
import Language.Haskell.LSP.Test.Session

data State = State String (FromServerMessage -> Bool) [FromClientMessage] State
           | Passed
           | Failed

data Event = Timeout | Received FromServerMessage

advance :: State -> Event -> Session State
advance _ Timeout = return Failed
advance s@(State name f outMsgs next) (Received msg)
  | f msg = do
    liftIO $ putStrLn name
    mapM_ (handleClientMessage sendRequestMessage sendMessage sendMessage) outMsgs
    return next
  | otherwise = return s
advance s _ = return s

mkStates [] = Passed
mkStates ((n, f, msgs):xs) = State n f msgs (mkStates xs)

main = let symbReq = ReqDocumentSymbols (RequestMessage "2.0" (IdInt 24) TextDocumentDocumentSymbol (DocumentSymbolParams (TextDocumentIdentifier (filePathToUri "/Users/luke/Desktop/test/src/Lib.hs"))))
           barPred (RspDocumentSymbols _) = True
           barPred _ = False
           encoded = [("start", const True, [symbReq])
                     ,("silent", barPred, [])
                     ,("end", const True, [])]
           initState = mkStates encoded
        in
          runSession "hie --lsp" "/Users/luke/Desktop/test" $ do
            openDoc "src/Lib.hs" "haskell"
            let f Passed = return Passed
                f s = Received <$> anyMessage >>= advance s >>= f
            res <- f initState
            case res of
              Passed -> return "passed"
              _ -> return "failed"

