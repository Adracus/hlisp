module HLisp.Data
( Atom (..),
  Bindings,
  HFn,
  Res,
  unit,
) where

import Text.Printf
import Data.Map (Map)

type Bindings = Map String Atom
type Res = (Bindings, Atom)
type HFn = Bindings -> [Atom] -> Either String Res

data Atom = Str String
          | Number Integer
          | Ident String
          | List [Atom]
          | SpecialForm HFn

unit :: Atom
unit = List []

instance Show Atom where
  show (Str s)          = printf "\"%s\"" s
  show (Number n)       = show n
  show (Ident i)        = i
  show (List l)         = printf "(%s)" $ unwords $ map show l
  show (SpecialForm _)  = "<function>"

