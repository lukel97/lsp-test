module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.HashMap.Lazy             as HM
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test.Script
import           System.Environment
import           System.FilePath
import           System.Directory
import           System.Exit
import           Language.Haskell.LSP.Test.Machine
import           Language.Haskell.LSP.Test.Parsing
                                                ( toJSONMsg )
import           Language.Haskell.LSP.Test.Replay
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types    as LSP

main = do
  args <- getArgs
  curDir <- getCurrentDirectory
  case args of
    ["replay", cmd] -> replaySession cmd curDir
    [file, cmd] -> do
      blocks <- parseScript <$> readFile file
      success <- runBlocks cmd curDir blocks
      if success
        then putStrLn "Success ✅"
        else putStrLn "Failed ❌" >> exitFailure
    _ -> putStrLn "usage: lsp-test (replay <cmd>)|(<file> <cmd>)"

runBlocks :: String -> FilePath -> [Block] -> IO Bool
runBlocks cmd rootDir blocks = runMachine cmd rootDir (map convertBlock blocks)
  where
    convertBlock :: Block -> (String, FromServerMessage -> Bool, [Session ()])
    convertBlock (Block name w actions) = (name, mkWait w, map mkAction actions)

    mkWait :: Wait -> FromServerMessage -> Bool
    mkWait WaitAny _ = True
    mkWait (WaitPred preds) x = all (`mkPred` x) preds

    mkPred :: Predicate -> (FromServerMessage -> Bool)
    mkPred (Predicate accessor comparison) msg =
      let (Object obj) = toJSONMsg msg in comp (access obj accessor) comparison

    comp (Just (String str)) (EqualsString expected) = str == T.pack expected
    comp (Just (Number num)) (EqualsNumber expected) = num == expected
    comp _ _ = False

    access :: Object -> Accessor -> Maybe Value
    access obj (AccessorTerm prop) = HM.lookup (T.pack prop) obj
    access obj (Accessor prop next) =
      case HM.lookup (T.pack prop) obj of
        Just (Object nextObj) -> access nextObj next
        _ -> Nothing

    mkAction :: Action -> Session ()

    mkAction (OpenDoc fp fileType) = void $ openDoc fp fileType

    mkAction (Request identifier methodStr ps) = void $ sendRequest' (strToMethod methodStr) (paramToValue ps)
    mkAction (Reply methodStr ps) = undefined -- TODO
    mkAction (Notify methodStr ps) = void $ sendNotification (strToMethod methodStr) (paramToValue ps)

    strToMethod str = case fromJSON (String $ T.pack str) of
      Success x -> x
      Error _ -> error $ str ++ " is not a valid method"
    paramToValue (ParamString str) = String str
    paramToValue (ParamUri uri)    = toJSON $ LSP.filePathToUri (rootDir </> uri)
    paramToValue (ParamObject obj) = Object (HM.map paramToValue obj)