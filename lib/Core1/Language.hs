-- |
module Core1.Language where

import Data.Set (Set, fromList, member)

type Name = String

type IsRec = Bool

type Alter a = (Int, [a], Expr a) -- one clause of a case expression: <1> a b -> e

-- We choose to parameterise the data type of expr with respect to its binders.
-- A binder is the name used at the binding occurrence of a variable.
data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int -- Pack{tag, arity}: tag is an integer which uniquely identifies the constructor, and arity tells how many arguments it takes.
  | EAp (Expr a) (Expr a)
  | ELet IsRec [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)

-- Super combinator
type ScDefn a = (a, [a], Expr a) -- name, args, body

-- The program is a collection of supercombinator definitions
type Program a = [ScDefn a]

type CoreExpr = Expr Name

type CoreScDefn = ScDefn Name

type CoreAlter = Alter Name

-- A Core program consists of a set of supercombinator definitions, including a distinguished one, main
type CoreProgram = Program Name

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e = False

-- Prelude
-- I x = x ;
-- K x y = x ;
-- K1 x y = y ;
-- S f g x = f x (g x) ;
-- compose f g x = f (g x) ;
-- twice f = compose f f

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ( "S",
      ["f", "g", "x"],
      EAp
        (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x"))
    ),
    ( "compose",
      ["f", "g", "x"],
      EAp
        (EVar "f")
        (EAp (EVar "g") (EVar "x"))
    ),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]
