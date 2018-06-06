module HLisp.Stdlib where

import HLisp.Data
import qualified HLisp.Eval as Eval
import qualified Data.Map as Map

def :: Atom
def = SpecialForm (\b [Ident i, v] -> fmap (\(b, v) -> (Map.insert i v b, v)) $ Eval.eval b v)

eval :: Atom
eval = SpecialForm (\b [a] -> Eval.eval b a)

if' :: Atom
if' = SpecialForm (\b [cond', then', else'] -> do
  (b, Boolean cond) <- Eval.eval b cond'
  if cond then Eval.eval b then' else Eval.eval b else')

let' :: Atom
let' = SpecialForm (\b [List [Ident i, v], body] -> do
  (b, v) <- Eval.eval b v
  (b, r) <- Eval.eval (Map.insert i v b) body
  return (Map.delete i b, r))

err :: Atom
err = HFunction (\b [Str msg] -> Left msg)

plus :: Atom
plus = HFunction (\b [Number n1, Number n2] -> Right (b, Number $ n1 + n2))

true :: Atom
true = Boolean True

false :: Atom
false = Boolean False

stdlib :: Bindings
stdlib =
  Map.fromList [
    ("def", def),
    ("eval", eval),
    ("if", if'),
    ("let", let'),

    ("plus", plus),
    ("err", err),

    ("true", true),
    ("false", false)
  ]

