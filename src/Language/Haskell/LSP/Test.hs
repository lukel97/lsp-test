{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Language.Haskell.LSP.Test
-- Description : A functional testing framework for LSP servers.
-- Maintainer  : luke_lau@icloud.com
-- Stability   : experimental
--
-- A framework for testing
-- <https://github.com/Microsoft/language-server-protocol Language Server Protocol servers>
-- functionally.

module Language.Haskell.LSP.Test
  (
  -- * Sessions
    runSession
  , runSessionWithHandles
  , runSessionWithConfig
  , SessionT
  , SessionConfig(..)
  , SessionException(..)
  , anySessionException
  , withTimeout
  -- * Capabilities
  , fullCaps
  -- * Sending
  , sendRequest
  , sendRequest_
  , sendRequest'
  , sendNotification
  , sendRequestMessage
  , sendNotification'
  , sendResponse
  -- * Receving
  , message
  , anyRequest
  , anyResponse
  , anyNotification
  , anyMessage
  , loggingNotification
  , publishDiagnosticsNotification
  -- * Combinators
  , satisfy
  -- * Utilities
  , initializeResponse
  -- ** Documents
  , openDoc
  , closeDoc
  , getOpenDocs
  , documentContents
  , getDocumentEdit
  , getDocUri
  , getVersionedDoc
  -- ** Symbols
  , getDocumentSymbols
  -- ** Diagnostics
  , waitForDiagnostics
  , waitForDiagnosticsSource
  , noDiagnostics
  -- ** Commands
  , executeCommand
  -- ** Code Actions
  , getAllCodeActions
  , executeCodeAction
  -- ** Completions
  , getCompletions
  -- ** References
  , getReferences
  -- ** Definitions
  , getDefinitions
  -- ** Renaming
  , rename
  -- ** Hover
  , getHover
  -- ** Highlights
  , getHighlights
  -- ** Formatting
  , formatDoc
  , formatRange
  -- ** Edits
  , applyEdit
  ) where

import Conduit (MonadThrow)
import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Lens hiding ((.=), List)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Language.Haskell.LSP.Types hiding (id, capabilities, message)
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Test.Capabilities
import Language.Haskell.LSP.Test.Compat
import Language.Haskell.LSP.Test.Decoding
import Language.Haskell.LSP.Test.Exceptions
import Language.Haskell.LSP.Test.Parsing
import Language.Haskell.LSP.Test.Session
import Language.Haskell.LSP.Test.Server
import System.IO
import System.Directory
import System.FilePath
import qualified Yi.Rope as Rope

-- | Starts a new session.
runSession :: (MonadIO m, MonadThrow m)
           => String -- ^ The command to run the server.
           -> LSP.ClientCapabilities -- ^ The capabilities that the client should declare.
           -> FilePath -- ^ The filepath to the root directory for the session.
           -> SessionT m a -- ^ The session to run.
           -> m a
runSession = runSessionWithConfig def

-- | Starts a new sesion with a client with the specified capabilities.
runSessionWithConfig :: forall m a. (MonadIO m, MonadThrow m)
                     => SessionConfig -- ^ Configuration options for the session.
                     -> String -- ^ The command to run the server.
                     -> LSP.ClientCapabilities -- ^ The capabilities that the client should declare.
                     -> FilePath -- ^ The filepath to the root directory for the session.
                     -> SessionT m a -- ^ The session to run.
                     -> m a
runSessionWithConfig config serverExe caps rootDir session = do
  pid <- liftIO getCurrentProcessID
  absRootDir <- liftIO $ canonicalizePath rootDir

  let initializeParams = InitializeParams (Just pid)
                                          (Just $ T.pack absRootDir)
                                          (Just $ filePathToUri absRootDir)
                                          Nothing
                                          caps
                                          (Just TraceOff)
  withServer serverExe (logStdErr config) $ \serverIn serverOut _ ->
    runSessionWithHandles serverIn serverOut listenServer config caps rootDir $ do

      -- Wrap the session around initialize and shutdown calls
      initRspMsg <- sendRequest Initialize initializeParams :: SessionT m InitializeResponse

      liftIO $ maybe (return ()) (putStrLn . ("Error while initializing: " ++) . show ) (initRspMsg ^. LSP.error)

      initRspVar <- initRsp <$> ask
      liftIO $ putMVar initRspVar initRspMsg

      sendNotification Initialized InitializedParams

      -- Run the actual test
      result <- session

      sendNotification Exit ExitParams

      return result
  where
  -- | Listens to the server output, makes sure it matches the record and
  -- signals any semaphores
  listenServer :: Handle -> SessionContext -> IO ()
  listenServer serverOut context = do
    msgBytes <- getNextMessage serverOut

    reqMap <- readMVar $ requestMap context

    let msg = decodeFromServerMsg reqMap msgBytes
    writeChan (messageChan context) (ServerMessage msg)

    listenServer serverOut context

-- | The current text contents of a document.
documentContents :: MonadIO m => TextDocumentIdentifier -> SessionT m T.Text
documentContents doc = do
  vfs <- vfs <$> get
  let file = vfs Map.! (doc ^. uri)
  return $ Rope.toText $ Language.Haskell.LSP.VFS._text file

-- | Parses an ApplyEditRequest, checks that it is for the passed document
-- and returns the new content
getDocumentEdit :: forall m. MonadIO m => TextDocumentIdentifier -> SessionT m T.Text
getDocumentEdit doc = do
  req <- message :: SessionT m ApplyWorkspaceEditRequest

  unless (checkDocumentChanges req || checkChanges req) $
    liftIO $ throw (IncorrectApplyEditRequest (show req))

  documentContents doc
  where
    checkDocumentChanges :: ApplyWorkspaceEditRequest -> Bool
    checkDocumentChanges req =
      let changes = req ^. params . edit . documentChanges
          maybeDocs = fmap (fmap (^. textDocument . uri)) changes
      in case maybeDocs of
        Just docs -> (doc ^. uri) `elem` docs
        Nothing -> False
    checkChanges :: ApplyWorkspaceEditRequest -> Bool
    checkChanges req =
      let mMap = req ^. params . edit . changes
        in maybe False (HashMap.member (doc ^. uri)) mMap

-- | Sends a request to the server and waits for its response.
-- @
-- rsp <- sendRequest TextDocumentDocumentSymbol params :: SessionT m DocumentSymbolsResponse
-- @
-- Note: will skip any messages in between the request and the response.
sendRequest :: (MonadIO m, ToJSON params, FromJSON a) => ClientMethod -> params -> SessionT m (ResponseMessage a)
sendRequest m = sendRequest' m >=> skipManyTill anyMessage . responseForId

-- | Send a request to the server and wait for its response,
-- but discard it.
sendRequest_ :: forall m params. (MonadIO m, ToJSON params) => ClientMethod -> params -> SessionT m ()
sendRequest_ p = void . (sendRequest p :: ToJSON params => params -> SessionT m (ResponseMessage Value))

-- | Sends a request to the server without waiting on the response.
sendRequest'
  :: (ToJSON params, MonadIO m)
  => ClientMethod -- ^ The request method.
  -> params -- ^ The request parameters.
  -> SessionT m LspId -- ^ The id of the request that was sent.
sendRequest' method params = do
  id <- curReqId <$> get
  modify $ \c -> c { curReqId = nextId id }

  let req = RequestMessage' "2.0" id method params

  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ updateRequestMap r id method

  sendMessage req

  return id

  where nextId (IdInt i) = IdInt (i + 1)
        nextId (IdString s) = IdString $ T.pack $ show $ read (T.unpack s) + 1

-- | A custom type for request message that doesn't
-- need a response type, allows us to infer the request
-- message type without using proxies.
data RequestMessage' a = RequestMessage' T.Text LspId ClientMethod a

instance ToJSON a => ToJSON (RequestMessage' a) where
  toJSON (RequestMessage' rpc id method params) =
    object ["jsonrpc" .= rpc, "id" .= id, "method" .= method, "params" .= params]


sendRequestMessage :: (MonadIO m, ToJSON a, ToJSON b) => RequestMessage ClientMethod a b -> SessionT m ()
sendRequestMessage req = do
  -- Update the request map
  reqMap <- requestMap <$> ask
  liftIO $ modifyMVar_ reqMap $
    \r -> return $ updateRequestMap r (req ^. LSP.id) (req ^. method)

  sendMessage req

-- | Sends a notification to the server.
sendNotification :: (MonadIO m, ToJSON a)
                 => ClientMethod -- ^ The notification method.
                 -> a -- ^ The notification parameters.
                 -> SessionT m ()

-- | Open a virtual file if we send a did open text document notification
sendNotification TextDocumentDidOpen params = do
  let params' = fromJust $ decode $ encode params
      n :: DidOpenTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidOpen params'
  oldVFS <- vfs <$> get
  newVFS <- liftIO $ openVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendNotification' n

-- | Close a virtual file if we send a close text document notification
sendNotification TextDocumentDidClose params = do
  let params' = fromJust $ decode $ encode params
      n :: DidCloseTextDocumentNotification
      n = NotificationMessage "2.0" TextDocumentDidClose params'
  oldVFS <- vfs <$> get
  newVFS <- liftIO $ closeVFS oldVFS n
  modify (\s -> s { vfs = newVFS })
  sendNotification' n

sendNotification method params = sendNotification' (NotificationMessage "2.0" method params)

sendNotification' :: (MonadIO m, ToJSON a, ToJSON b) => NotificationMessage a b -> SessionT m ()
sendNotification' = sendMessage

sendResponse :: (MonadIO m, ToJSON a) => ResponseMessage a -> SessionT m ()
sendResponse = sendMessage

-- | Returns the initialize response that was received from the server.
-- The initialize requests and responses are not included the session,
-- so if you need to test it use this.
initializeResponse :: MonadIO m => SessionT m InitializeResponse
initializeResponse = initRsp <$> ask >>= (liftIO . readMVar)

-- | Opens a text document and sends a notification to the client.
openDoc :: MonadIO m => FilePath -> String -> SessionT m TextDocumentIdentifier
openDoc file languageId = do
  item <- getDocItem file languageId
  sendNotification TextDocumentDidOpen (DidOpenTextDocumentParams item)
  TextDocumentIdentifier <$> getDocUri file
  where
  -- | Reads in a text document as the first version.
  getDocItem :: MonadIO m
             =>FilePath -- ^ The path to the text document to read in.
             -> String -- ^ The language ID, e.g "haskell" for .hs files.
             -> SessionT m TextDocumentItem
  getDocItem file languageId = do
    context <- ask
    let fp = rootDir context </> file
    contents <- liftIO $ T.readFile fp
    return $ TextDocumentItem (filePathToUri fp) (T.pack languageId) 0 contents

-- | Closes a text document and sends a notification to the client.
closeDoc :: MonadIO m => TextDocumentIdentifier -> SessionT m ()
closeDoc docId = do
  let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. uri))
  sendNotification TextDocumentDidClose params

  oldVfs <- vfs <$> get
  let notif = NotificationMessage "" TextDocumentDidClose params
  newVfs <- liftIO $ closeVFS oldVfs notif
  modify $ \s -> s { vfs = newVfs }

getOpenDocs :: MonadIO m => SessionT m [TextDocumentIdentifier]
getOpenDocs = map TextDocumentIdentifier . Map.keys . vfs <$> get

-- | Gets the Uri for the file corrected to the session directory.
getDocUri :: MonadIO m => FilePath -> SessionT m Uri
getDocUri file = do
  context <- ask
  let fp = rootDir context </> file
  return $ filePathToUri fp

-- | Waits for diagnostics to be published and returns them.
waitForDiagnostics :: forall m. MonadIO m => SessionT m [Diagnostic]
waitForDiagnostics = do
  diagsNot <- skipManyTill anyMessage message :: SessionT m PublishDiagnosticsNotification
  let (List diags) = diagsNot ^. params . LSP.diagnostics
  return diags

waitForDiagnosticsSource :: MonadIO m => String -> SessionT m [Diagnostic]
waitForDiagnosticsSource src = do
  diags <- waitForDiagnostics
  let res = filter matches diags
  if null res
    then waitForDiagnosticsSource src
    else return res
  where
    matches :: Diagnostic -> Bool
    matches d = d ^. source == Just (T.pack src)

-- | Expects a 'PublishDiagnosticsNotification' and throws an
-- 'UnexpectedDiagnosticsException' if there are any diagnostics
-- returned.
noDiagnostics :: forall m. MonadIO m => SessionT m ()
noDiagnostics = do
  diagsNot <- message :: SessionT m PublishDiagnosticsNotification
  when (diagsNot ^. params . LSP.diagnostics /= List []) $ liftIO $ throw UnexpectedDiagnostics

-- | Returns the symbols in a document.
getDocumentSymbols :: MonadIO m => TextDocumentIdentifier -> SessionT m [SymbolInformation]
getDocumentSymbols doc = do
  ResponseMessage _ rspLid mRes mErr <- sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
  maybe (return ()) (throw . UnexpectedResponseError rspLid) mErr
  let (Just (List symbols)) = mRes
  return symbols

-- | Returns all the code actions in a document by 
-- querying the code actions at each of the current 
-- diagnostics' positions.
getAllCodeActions :: forall m. MonadIO m => TextDocumentIdentifier -> SessionT m [CommandOrCodeAction]
getAllCodeActions doc = do
  curDiags <- fromMaybe [] . Map.lookup (doc ^. uri) . curDiagnostics <$> get
  let ctx = CodeActionContext (List curDiags) Nothing

  foldM (go ctx) [] curDiags

  where
    go :: CodeActionContext -> [CommandOrCodeAction] -> Diagnostic -> SessionT m [CommandOrCodeAction]
    go ctx acc diag = do
      ResponseMessage _ rspLid mRes mErr <- sendRequest TextDocumentCodeAction (CodeActionParams doc (diag ^. range) ctx)

      case mErr of
        Just e -> throw (UnexpectedResponseError rspLid e)
        Nothing ->
          let Just (List cmdOrCAs) = mRes
            in return (acc ++ cmdOrCAs)

-- | Executes a command.
executeCommand :: MonadIO m => Command -> SessionT m ()
executeCommand cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams (cmd ^. command) args
  sendRequest_ WorkspaceExecuteCommand execParams

-- | Executes a code action. 
-- Matching with the specification, if a code action
-- contains both an edit and a command, the edit will
-- be applied first.
executeCodeAction :: forall m. MonadIO m => CodeAction -> SessionT m ()
executeCodeAction action = do
  maybe (return ()) handleEdit $ action ^. edit
  maybe (return ()) executeCommand $ action ^. command

  where handleEdit :: WorkspaceEdit -> SessionT m ()
        handleEdit e =
          -- Its ok to pass in dummy parameters here as they aren't used
          let req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams e)
            in updateState (ReqApplyWorkspaceEdit req)

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: MonadIO m => TextDocumentIdentifier -> SessionT m VersionedTextDocumentIdentifier
getVersionedDoc (TextDocumentIdentifier uri) = do
  fs <- vfs <$> get
  let ver =
        case fs Map.!? uri of
          Just (VirtualFile v _) -> Just v
          _ -> Nothing
  return (VersionedTextDocumentIdentifier uri ver)

-- | Applys an edit to the document and returns the updated document version.
applyEdit :: MonadIO m => TextDocumentIdentifier -> TextEdit -> SessionT m VersionedTextDocumentIdentifier
applyEdit doc edit = do

  verDoc <- getVersionedDoc doc

  caps <- asks sessionCapabilities

  let supportsDocChanges = fromMaybe False $ do
        let LSP.ClientCapabilities mWorkspace _ _ = caps
        LSP.WorkspaceClientCapabilities _ mEdit _ _ _ _ <- mWorkspace
        LSP.WorkspaceEditClientCapabilities mDocChanges <- mEdit
        mDocChanges

  let wEdit = if supportsDocChanges
      then
        let docEdit = TextDocumentEdit verDoc (List [edit])
        in WorkspaceEdit Nothing (Just (List [docEdit]))
      else
        let changes = HashMap.singleton (doc ^. uri) (List [edit])
        in WorkspaceEdit (Just changes) Nothing

  let req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams wEdit)
  updateState (ReqApplyWorkspaceEdit req)

  -- version may have changed
  getVersionedDoc doc
  
-- | Returns the completions for the position in the document.
getCompletions :: MonadIO m => TextDocumentIdentifier -> Position -> SessionT m [CompletionItem]
getCompletions doc pos = do
  rsp <- sendRequest TextDocumentCompletion (TextDocumentPositionParams doc pos)

  case getResponseResult rsp of
    Completions (List items) -> return items
    CompletionList (CompletionListType _ (List items)) -> return items

-- | Returns the references for the position in the document.
getReferences :: MonadIO m
              => TextDocumentIdentifier -- ^ The document to lookup in.
              -> Position -- ^ The position to lookup. 
              -> Bool -- ^ Whether to include declarations as references.
              -> SessionT m [Location] -- ^ The locations of the references.
getReferences doc pos inclDecl =
  let ctx = ReferenceContext inclDecl
      params = ReferenceParams doc pos ctx
  in getResponseResult <$> sendRequest TextDocumentReferences params 

-- | Returns the definition(s) for the term at the specified position.
getDefinitions :: MonadIO m
               => TextDocumentIdentifier -- ^ The document the term is in.
               -> Position -- ^ The position the term is at.
               -> SessionT m [Location] -- ^ The location(s) of the definitions
getDefinitions doc pos =
  let params = TextDocumentPositionParams doc pos
  in getResponseResult <$> sendRequest TextDocumentDefinition params

-- ^ Renames the term at the specified position.
rename :: forall m. MonadIO m => TextDocumentIdentifier -> Position -> String -> SessionT m ()
rename doc pos newName = do
  let params = RenameParams doc pos (T.pack newName)
  rsp <- sendRequest TextDocumentRename params :: SessionT m RenameResponse
  let wEdit = getResponseResult rsp
      req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams wEdit)
  updateState (ReqApplyWorkspaceEdit req)

-- | Returns the hover information at the specified position.
getHover :: MonadIO m => TextDocumentIdentifier -> Position -> SessionT m (Maybe Hover)
getHover doc pos =
  let params = TextDocumentPositionParams doc pos
  in getResponseResult <$> sendRequest TextDocumentHover params

-- | Returns the highlighted occurences of the term at the specified position
getHighlights :: MonadIO m => TextDocumentIdentifier -> Position -> SessionT m [DocumentHighlight]
getHighlights doc pos =
  let params = TextDocumentPositionParams doc pos
  in getResponseResult <$> sendRequest TextDocumentDocumentHighlight params

-- | Checks the response for errors and throws an exception if needed.
-- Returns the result if successful.
getResponseResult :: ResponseMessage a -> a 
getResponseResult rsp = fromMaybe exc (rsp ^. result)
  where exc = throw $ UnexpectedResponseError (rsp ^. LSP.id)
                                              (fromJust $ rsp ^. LSP.error)

-- | Applies formatting to the specified document.
formatDoc :: MonadIO m => TextDocumentIdentifier -> FormattingOptions -> SessionT m ()
formatDoc doc opts = do
  let params = DocumentFormattingParams doc opts
  edits <- getResponseResult <$> sendRequest TextDocumentFormatting params
  applyTextEdits doc edits

-- | Applies formatting to the specified range in a document.
formatRange :: MonadIO m => TextDocumentIdentifier -> FormattingOptions -> Range -> SessionT m ()
formatRange doc opts range = do
  let params = DocumentRangeFormattingParams doc range opts
  edits <- getResponseResult <$> sendRequest TextDocumentRangeFormatting params
  applyTextEdits doc edits

applyTextEdits :: MonadIO m => TextDocumentIdentifier -> List TextEdit -> SessionT m ()
applyTextEdits doc edits =
  let wEdit = WorkspaceEdit (Just (HashMap.singleton (doc ^. uri) edits)) Nothing
      req = RequestMessage "" (IdInt 0) WorkspaceApplyEdit (ApplyWorkspaceEditParams wEdit)
  in updateState (ReqApplyWorkspaceEdit req)

