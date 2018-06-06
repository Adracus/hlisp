module HLisp.Eval (
  eval,
  evalProgram
) where

import HLisp.Data
import Text.Printf
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Map as Map

eval :: Bindings -> Atom -> Either String Res
eval b a =
  case a of
    Str _          -> Right (b, a)
    Number _       -> Right (b, a)
    SpecialForm _  -> Right (b, a)
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

    evalArgs :: Bindings -> [Atom] -> Either String (Bindings, [Atom])
    evalArgs b args = foldM f (b, []) args
      where f (b, acc) a = fmap (\(b, a) -> (b, acc ++ [a])) $ eval b a

    evalOp :: Bindings -> Atom -> [Atom] -> Either String Res
    evalOp b (SpecialForm m) args = m b args
    evalOp b (HFunction f) args   = (evalArgs b args) >>= (uncurry f)
    evalOp _ other _              = Left $ printf "Cannot eval '%s'" (show other)

evalProgram :: Bindings -> [Atom] -> Either String Atom
evalProgram b atoms = fmap snd $ foldM (eval . fst) (b, unit) atoms

