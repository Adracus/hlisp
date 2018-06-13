module HLisp.Stdlib where

import Text.Printf
import HLisp.Data
import qualified HLisp.Eval as Eval
import qualified Data.Map as Map

type HFunction0 = Bindings -> Either String Res
type HFunction1 = Bindings -> Atom -> Either String Res
type HFunction2 = Bindings -> Atom -> Atom -> Either String Res
type HFunction3 = Bindings -> Atom -> Atom -> Atom -> Either String Res

arityN :: Int -> HFunction -> HFunction
arityN n f = \b args ->
  let len = length args in
    if len == n
    then f b args
    else Left $ printf "Invalid arity: Expected %d arguments but got %d arguments" n len

arity0 :: HFunction0 -> HFunction
arity0 f = arityN 0 inner
  where inner b [] = f b
        inner b _  = error "Invalid state"

arity1 :: HFunction1 -> HFunction
arity1 f = arityN 1 inner
  where inner b [a1] = f b a1
        inner b _    = error "Invalid state"

arity2 :: HFunction2 -> HFunction
arity2 f = arityN 2 inner
  where inner b [a1, a2] = f b a1 a2
        inner b _        = error "Invalid state"

arity3 :: HFunction3 -> HFunction
arity3 f = arityN 3 inner
  where inner b [a1, a2, a3] = f b a1 a2 a3
        inner b _            = error "Invalid state"

defFn :: HFunction2
defFn b (Ident i) v = fmap (\(b, v) -> (Map.insert i v b, v)) $ Eval.eval b v

evalFn :: HFunction1
evalFn b a = Eval.eval b a

ifFn :: HFunction3
ifFn b cond' then' else' = do
  (b, Boolean cond) <- Eval.eval b cond'
  if cond then Eval.eval b then' else Eval.eval b else'

letFn :: HFunction
letFn b [List [Ident i, v], body] = do
  (b, v) <- Eval.eval b v
  (b, r) <- Eval.eval (Map.insert i v b) body
  return (Map.delete i b, r)

errFn :: HFunction1
errFn b (Str msg) = Left msg

plusFn :: HFunction2
plusFn b (Number n1) (Number n2) = Right (b, Number $ n1 + n2)

def :: Atom
def = SpecialForm $ arity2 defFn

eval :: Atom
eval = SpecialForm $ arity1 evalFn

if' :: Atom
if' = SpecialForm $ arity3 ifFn

let' :: Atom
let' = SpecialForm letFn

err :: Atom
err = Builtin $ arity1 errFn

plus :: Atom
plus = Builtin $ arity2 plusFn

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

