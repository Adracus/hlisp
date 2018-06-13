module HLisp.Data
( Atom (..),
  Bindings,
  HFunction,
  Res,
  unit,
) where

import Text.Printf
import Data.Map (Map)

type Bindings = Map String Atom
type Res = (Bindings, Atom)
type HFunction = (Bindings -> [Atom] -> Either String Res)

data Atom = Str String
          | Number Integer
          | Boolean Bool
          | Ident String
          | List [Atom]
          | SpecialForm HFunction
          | Builtin HFunction

unit :: Atom
unit = List []

instance Show Atom where
  show (Str s)          = printf "\"%s\"" s
  show (Number n)       = show n
  show (Boolean b)      = show b
  show (Ident i)        = i
  show (List l)         = printf "(%s)" $ unwords $ map show l
  show (Builtin _)      = "<function>"
  show (SpecialForm _)  = "<special form>"

