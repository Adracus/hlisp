module Main where

import Text.Parsec
import Text.Printf
import Text.Show.Functions
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map as Map

type Parser a = Parsec String () a
type Bindings = Map.Map String Atom
type Res = (Bindings, Atom)
type HFn = Bindings -> [Atom] -> Either String Res

data Atom = Str String
          | Number Integer
          | Ident String
          | List [Atom]
          | SpecialForm HFn

instance Show Atom where
  show (Str s)    = printf "\"%s\"" s
  show (Number n) = show n
  show (Ident i)  = i
  show (List l)   = printf "(%s)" $ unwords $ map show l
  show (SpecialForm _)     = "<function>"

unit = List []

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseStr :: Parser Atom
parseStr = do
  char '"'
  strings <- many character
  char '"'
  return $ Str $ concat strings

parseNumber :: Parser Atom
parseNumber = fmap (Number . read) $ many1 digit

parseIdent :: Parser Atom
parseIdent = do
  s <- letter
  r <- many alphaNum
  return $ Ident $ s:r

parseAtom :: Parser Atom
parseAtom = parseStr <|> parseNumber <|> parseIdent

parseList :: Parser Atom
parseList = do
  char '('
  exprs <- many parseSExpr
  char ')'
  return $ List exprs

parseSExpr :: Parser Atom
parseSExpr = between spaces spaces $ parseAtom <|> parseList

parseProgram :: Parser [Atom]
parseProgram = manyTill parseSExpr eof

eval :: Bindings -> Atom -> Either String Res
eval b a =
  case a of
    Str _          -> Right (b, a)
    Number _       -> Right (b, a)
    SpecialForm _  -> Left "Cannot take value of macro"
    List []        -> Right (b, a)
    Ident i        -> evalIdent b i
    List (op:args) -> do
      (b, op) <- eval b op
      evalOp b op args
  where
    evalIdent :: Bindings -> String -> Either String Res
    evalIdent b i =
      fromMaybe
        (Left (printf "Undefined '%s'" i)) $
        fmap (\x -> Right (b, x)) $ b Map.!? i

    evalOp :: Bindings -> Atom -> [Atom] -> Either String Res
    evalOp b (SpecialForm m) args = m b args
    evalOp _ other _              = Left $ printf "Cannot eval '%s'" (show other)

evalProgram :: Bindings -> [Atom] -> Either String Atom
evalProgram b atoms = fmap snd $ foldM (eval . fst) (b, unit) atoms

def :: HFn
def b ((Ident i):v:[]) = Right (Map.insert i v b, v)

plus :: HFn
plus b ((Number n1):(Number n2):_) = Right (b, Number $ n1 + n2)

evalFn :: HFn
evalFn b [a] = eval b a

defaultBindings :: Map.Map String Atom
defaultBindings =
  Map.fromList [
    ("def", SpecialForm def),
    ("plus", SpecialForm plus),
    ("eval", SpecialForm evalFn)
  ]

main :: IO ()
main = do
  contents <- getContents
  case parse parseProgram "" contents of
    Left err      -> putStrLn $ show err
    Right program -> case evalProgram defaultBindings program of
      Left err  -> putStrLn $ show err
      Right res -> putStrLn $ show res

