module Main where

import Control.Applicative ( (<|>), some )
import Control.Monad
import Data.Aeson
import Data.Char
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Scientific
import Text.ParserCombinators.ReadP
import System.Environment
import System.FilePath
import System.Directory
import Language.Haskell.LSP.Test (openDoc, sendRequest', sendNotification, sendResponse)
import Language.Haskell.LSP.Test.Session
import Language.Haskell.LSP.Test.Machine
import Language.Haskell.LSP.Test.Parsing (toJSONMsg)
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.TH.MessageFuncs
import qualified Language.Haskell.LSP.Types as LSP
import Debug.Trace

data Block = Block String Wait [Action]
  deriving Show

data Wait = WaitPred [Predicate]
          | WaitAny
  deriving Show

data Predicate = Predicate Accessor Comparison
  deriving Show

data Accessor = AccessorTerm String
              | Accessor String Accessor
  deriving Show

data Comparison = EqualsNumber Scientific
                | EqualsString String
                | ContainsString String
  deriving Show

data Action = OpenDoc FilePath String
            | Request String Method MessageParam
            | Reply Method MessageParam
            | Notify Method MessageParam
  deriving Show

type Method = String

data MessageParam = ParamObject (HM.HashMap T.Text MessageParam)
                  | ParamString T.Text
                  | ParamUri FilePath
  deriving Show

-- | Parse a string literal like "foo".
strLiteral :: ReadP String
strLiteral = between (char '"') (char '"') (many (satisfy (/= '"')))

-- | Parse mandatory whitespace, including newlines
space :: ReadP ()
space = void $ some (satisfy isSpace)

block :: ReadP Block
block = do
  skipSpaces
  name <- strLiteral
  skipSpaces
  between (char '{') (char '}') $ do
    skipSpaces
    w <- wait
    actions <- option [] $ do
      space
      string "then"
      space
      action `sepBy1` space
    skipSpaces
    return $ Block name w actions

wait :: ReadP Wait
wait = do
  string "wait for"
  space
  f <|> g
  where f = string "any" >> return WaitAny
        g = WaitPred <$> some predicate

predicate :: ReadP Predicate
predicate = do
  x <- accessor
  Predicate x <$> comparison

accessor :: ReadP Accessor
accessor = do
  x:xs <- reverse <$> sepBy1 property (char '.')
  return $ foldl (flip Accessor) (AccessorTerm x) xs
  where property = many (satisfy isAlphaNum)

comparison :: ReadP Comparison
comparison = do
  space
  operator <- string "==" <|> string "is in"
  space
  choice [eqString, eqNumber]
  -- todo: contains string
  where eqString = EqualsString <$> strLiteral
        eqNumber = EqualsNumber . read <$> some (satisfy isNumber)

action :: ReadP Action
action = choice
    [ openAction
    , requestAction
    , sendAction "reply"   Reply
    , sendAction "notify"  Notify
    ]
  where
    requestAction = do
      skipSpaces
      identifier <- manyTill (satisfy isAlphaNum) (skipSpaces >> char ':')
      skipSpaces
      sendAction "request" (Request identifier)

openAction :: ReadP Action
openAction = do
  skipSpaces
  string "open"
  space
  fp <- strLiteral
  space
  OpenDoc fp <$> strLiteral

sendAction :: String -> (String -> MessageParam -> Action) -> ReadP Action
sendAction keyword con = do
  skipSpaces
  string keyword
  skipSpaces
  method <- strLiteral
  skipSpaces
  con method <$> messageParam

messageParam :: ReadP MessageParam
messageParam = choice [uriParam, stringParam, objParam]
  where
    uriParam = do
      skipSpaces
      string "uri"
      skipSpaces
      fp <- strLiteral
      skipSpaces
      return (ParamUri fp)

    stringParam = ParamString . T.pack <$> strLiteral

    objParam = do
      props <- between (char '{') (char '}') (some parseProp)
      return (ParamObject (HM.fromList props))

    parseProp = do
      skipSpaces
      name <- many (satisfy (\x -> (x /= ':') && isAlphaNum x))
      char ':'
      skipSpaces
      param <- messageParam
      skipSpaces
      return (T.pack name, param)

parseScript :: String -> [Block]
parseScript str =
  case readP_to_S parser str of
    [] -> error "Couldn't parse"
    xs -> fst $ last xs
  where
    parser = do
      blocks <- some block
      skipSpaces
      eof
      return blocks

main = do
  fileName <- head <$> getArgs
  blocks <- parseScript <$> readFile fileName
  print blocks
  rootDir <- getCurrentDirectory
  runBlocks rootDir blocks


runBlocks :: FilePath -> [Block] -> IO ()
runBlocks rootDir blocks = runMachine rootDir (map convertBlock blocks) >>= putStrLn
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
    access obj (AccessorTerm prop) = traceShowId $ HM.lookup (T.pack prop) obj
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
