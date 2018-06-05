module Main where

import HLisp.Data
import HLisp.Parser
import Text.Printf
import Text.Show.Functions
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map as Map

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
  case parse contents of
    Left err      -> putStrLn $ show err
    Right program -> case evalProgram defaultBindings program of
      Left err  -> putStrLn $ show err
      Right res -> putStrLn $ show res

