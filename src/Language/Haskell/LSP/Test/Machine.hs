{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Language.Haskell.LSP.Test.Machine where

import Control.Monad.Catch
import Data.Default
import Language.Haskell.LSP.Test
import qualified Language.Haskell.LSP.Types as L
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Debug.Trace

data ModelState (v :: * -> *) = TDocClose | TDocOpen | TDocWaited
  deriving (Eq, Ord, Show)

data OpenDoc (v :: * -> *) = OpenDoc
  deriving (Eq, Show)

instance HTraversable OpenDoc where
  htraverse _ OpenDoc = pure OpenDoc

s_openDoc_init :: (Monad n) => Command n PropertySession ModelState
s_openDoc_init =
  let gen TDocClose = Just $ pure OpenDoc
      gen _      = Nothing
      execute OpenDoc = openDoc "Format.hs" "haskell"
  in Command gen execute [
      Require $ \s OpenDoc -> s == TDocClose
    , Update $ \_s OpenDoc o -> TDocOpen
    , Ensure $ \before after OpenDoc o -> do
        before === TDocClose
        let L.TextDocumentIdentifier uri = o
        uri === L.Uri "file:///Users/luke/Source/haskell-lsp-test/test/data/Format.hs"
        after === TDocOpen
    ]

data WaitDiags (v :: * -> *) = WaitDiags
  deriving (Eq, Show)

instance HTraversable WaitDiags where
  htraverse _ WaitDiags = pure WaitDiags

s_diagnostics :: Monad n => Command n PropertySession ModelState
s_diagnostics =
  let gen TDocOpen = Just $ pure WaitDiags
      gen _        = Nothing
      execute WaitDiags = waitForDiagnostics
  in Command gen execute [
      Require $ \s WaitDiags -> s == TDocOpen
    , Update $ \s WaitDiags o -> TDocWaited
    , Ensure $ \before after WaitDiags o -> o === []
    ]

data CloseDoc (v :: * -> *) = CloseDoc
  deriving (Eq, Show)

instance HTraversable CloseDoc where
  htraverse _ CloseDoc = pure CloseDoc

s_closeDoc :: Monad n => Command n PropertySession ModelState
s_closeDoc =
  let gen TDocOpen   = Just $ pure CloseDoc
      gen TDocWaited = Just $ pure CloseDoc
      gen _        = Nothing
      execute CloseDoc = closeDoc (L.TextDocumentIdentifier (L.Uri "file:///Users/luke/Source/haskell-lsp-test/test/data/Format.hs"))
  in Command gen execute [
      Require $ \s CloseDoc -> s == TDocOpen || s == TDocWaited
    , Update $ \_s CloseDoc o -> TDocClose
    ]

type PropertySession = SessionT (PropertyT IO)

instance MonadThrow m => MonadCatch (SessionT m) where
  catch f h = f

instance MonadTest PropertySession where
  liftTest = lift . liftTest

initialState :: ModelState v
initialState = TDocClose

prop_doc :: Property
prop_doc = property $ do
  actions <- forAll $
    Gen.sequential (Range.constant 1 100) initialState
      [ s_openDoc_init
      , s_diagnostics
      , s_closeDoc
      ]
  runSessionWithConfig (def { logMessages = True }) "hie --lsp" def "test/data" $
    executeSequential initialState actions

