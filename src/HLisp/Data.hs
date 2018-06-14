module HLisp.Data
( Atom (..),
  Bindings (..),
  HFunction,
  Res,
  unit,
  get,
  push,
  pop,
  insertLet,
  insertDef,
  empty,
) where

import Text.Printf
import Control.Monad (msum)
import Control.Applicative ((<|>))
import qualified Data.Map as Map

type Res = (Bindings, Atom)
type HFunction = (Bindings -> [Atom] -> Either String Res)
type Binds = Map.Map String Atom

data Bindings = Bindings { defs :: Binds, lets :: [Binds] }

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

mapBindings :: (Binds -> Binds) -> ([Binds] -> [Binds]) -> Bindings -> Bindings
mapBindings fDefs fLets Bindings{defs=d, lets=l} = Bindings { defs = fDefs d, lets = fLets l }

mapDefs :: (Binds -> Binds) -> Bindings -> Bindings
mapDefs f = mapBindings f id

mapLets :: ([Binds] -> [Binds]) -> Bindings -> Bindings
mapLets f = mapBindings id f

withDefs :: Binds -> Bindings -> Bindings
withDefs d = mapDefs (const d)

withLets :: [Binds] -> Bindings -> Bindings
withLets l = mapLets (const l)

insertDef :: String -> Atom -> Bindings -> Bindings
insertDef k v = mapDefs (Map.insert k v)

insertLet :: String -> Atom -> Bindings -> Bindings
insertLet k v = mapLets inner
  where inner []      = error "Cannot insert into empty lets"
        inner (x:xs)  = (Map.insert k v x) : xs

push :: Bindings -> Bindings
push = mapLets (\lets -> Map.empty : lets)

pop :: Bindings -> Bindings
pop = mapLets inner
  where inner [] = error "Cannot pop from empty lets"
        inner (x:xs) = xs

get :: String -> Bindings -> Maybe Atom
get k Bindings{defs=d, lets=l} = getDef <|> getLet
  where
    lookup = (Map.!? k)
    getDef = lookup d
    getLet = msum $ lookup <$> l

empty :: Bindings
empty = Bindings { defs = Map.empty, lets = [] }

