module Main where

import Control.Applicative ( (<|>), some )
import Data.Char
import Text.ParserCombinators.ReadP
import System.Environment

{-
 - "asdf"
 -    wait for
 -      asdsdf == "asdf"
 -      adsf   == "adsf"
 -    send
 -      foo
 -      bar
 -
 -  str   ::= " char "
 -  wait  ::= wait for (pred+ | any)
 -  pred  ::= x == y
 -  send  ::= send msg+
 -  msg   ::= str
 -  block ::= str wait send?
 -}

data Block = Block String Wait (Maybe Send)
  deriving Show
data Wait = WaitPred [Predicate]
          | WaitAny
  deriving Show
data Predicate = Predicate String String
  deriving Show
data Send = Send [Message]
  deriving Show
type Message = String

skip = skipMany $ satisfy isSpace <|> char '\n' <|> char '\r'

strLit :: ReadP String
strLit = between (char '"') (char '"') (many (satisfy (/= '"')))

block :: ReadP Block
block = do
  skip
  name <- strLit
  skip
  w <- wait
  skip
  s <- option Nothing (Just <$> send)
  return $ Block name w s

wait :: ReadP Wait
wait = do
  string "wait for"
  skip
  f <|> g
  where f = string "any" >> return WaitAny
        g = WaitPred <$> some predicate

predicate :: ReadP Predicate
predicate = do
  skip
  x <- strLit
  skip
  string "=="
  skip
  y <- strLit
  return $ Predicate x y

send :: ReadP Send
send = do
  -- skip
  string "send"
  Send <$> some (skip >> strLit)

parseScript :: String -> [Block]
parseScript = fst . last . readP_to_S (some block)

main = do
  fileName <- head <$> getArgs
  print . parseScript =<< readFile fileName
