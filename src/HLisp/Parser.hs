module HLisp.Parser
( parse
) where

import HLisp.Data
import Text.Parsec hiding (parse, string)
import qualified Text.Parsec as Parsec

type Parser a = Parsec String () a

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

string :: Parser Atom
string = do
  char '"'
  strings <- many character
  char '"'
  return $ Str $ concat strings

number :: Parser Atom
number = fmap (Number . read) $ many1 digit

ident :: Parser Atom
ident = do
  s <- letter
  r <- many alphaNum
  return $ Ident $ s:r

atom :: Parser Atom
atom = string <|> number <|> ident

list :: Parser Atom
list = do
  char '('
  exprs <- many sExpr
  char ')'
  return $ List exprs

sExpr :: Parser Atom
sExpr = between spaces spaces $ atom <|> list

program :: Parser [Atom]
program = manyTill sExpr eof

parse :: String -> Either ParseError [Atom]
parse = Parsec.parse program ""

