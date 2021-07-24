-- |
module Language where

-- import Utils
import Data.Set (Set, fromList, member)

type Name = String
type CoreExpr = Expr Name
type IsRec = Bool

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

-- Type parameter `a` is the binding type (not the result type)
data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet IsRec [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)

-- Super combinator
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- The program is a collection of supercombinator definitions
type Program a = [ScDefn a]
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

-- Sample Program
-- main = double 21 ;
-- double x = x + x

sampleProgram :: CoreProgram
sampleProgram =
  [ ("main", [], EAp (EVar "double") (ENum 21))
  , ("double", ["x"], EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
  , ("sumDoubles",
     ["x", "y"],
     ELet
       nonRecursive
       [ ("doubledX", EAp (EVar "double") (EVar "x")),
         ("doubledY", EAp (EVar "double") (EVar "y"))
       ]
       (EAp (EAp (EVar "+") (EVar "doubledX")) (EVar "doubledY"))
    )
  ]

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

-- helper function for replicating arguments with currying
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (replicate n e2)

class Iseq iseq where
  iNil :: iseq
  iStr :: String -> iseq
  iNum :: Int -> iseq
  iFWNum :: Int -> Int -> iseq
  iLayn :: [iseq] -> iseq
  iAppend :: iseq -> iseq -> iseq
  iNewline :: iseq
  iIndent :: iseq -> iseq
  iDisplay :: iseq -> String

infixr 5 `iAppend`

data Iseqrep
  = INil
  | IStr String
  | IAppend Iseqrep Iseqrep
  | IIndent Iseqrep
  | INewline

instance Iseq Iseqrep where
  iNil = INil

  iStr [] = INil
  iStr s = case break (== '\n') s of
    (str, []) -> IStr str
    (str, _ : rest) -> IStr str `iAppend` iNewline `iAppend` iStr rest

  iNum = IStr . show

  iFWNum width n =
    iStr (space (width - length digits) ++ digits)
    where
      digits = show n

  iLayn seqs =
    iConcat (zipWith (curry layItem) [1 ..] seqs)
    where
      layItem (n, seq) = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]
  iAppend INil seq = seq
  iAppend seq INil = seq
  iAppend seq1 seq2 = IAppend seq1 seq2

  iNewline = INewline

  iIndent seq = IIndent seq

  iDisplay seq = flatten 0 [(seq, 0), (INewline, 0)]

flatten ::
  Int -> -- Current cursor
  [(Iseqrep, Int)] -> -- Work list. (work item, current indent-level)
  String -- Result
flatten col [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IStr s, indent) : seqs) = s ++ flatten (length s + col) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs) =
  '\n' : space indent ++ flatten indent seqs
flatten col ((IIndent seq, indent) : seqs) =
  flatten col ((seq, col) : seqs)

space :: Int -> String
space n = replicate n ' '

iConcat :: (Iseq iseq) => [iseq] -> iseq
iConcat = foldr iAppend iNil

iInterleave :: (Iseq iseq) => iseq -> [iseq] -> iseq
iInterleave sep [] = iNil
iInterleave sep [seq] = seq
iInterleave sep (seq : seqs) = seq `iAppend` sep `iAppend` iInterleave sep seqs

pprProgram :: CoreProgram -> Iseqrep
pprProgram scdefns = iInterleave iNewline $ map pprScDefn scdefns

pprScDefn :: CoreScDefn -> Iseqrep
pprScDefn (name, args, expr) =
  iConcat
    [ iStr name,
      iSpace,
      pprArgs args,
      iStr " = ",
      iIndent (pprExpr expr),
      iStr " ;"
    ]

iSpace :: Iseq iseq => iseq
iSpace = iStr " "

pprArgs :: [Name] -> Iseqrep
pprArgs args = iInterleave iSpace (map iStr args)

pprAExpr :: CoreExpr -> Iseqrep
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

infixOperators :: Set Name
infixOperators = fromList [ "+", "-", "*", "/", "==", "~=", ">", ">=", "<", "<=", "&", "|" ]

pprExpr :: CoreExpr -> Iseqrep
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp (EAp (EVar op) e1) e2) | op `member` infixOperators
  = iConcat [ pprAExpr e1, iSpace, iStr op, iSpace, pprAExpr e2 ]
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr) =
  iConcat
    [ iStr keyword,
      iStr " ",
      iIndent (pprDefns defns),
      iNewline,
      iStr "in ",
      pprExpr expr
    ]
  where
    keyword = if isrec then "letrec" else "let"
pprExpr (EConstr tag arity) =
  iConcat
    [ iStr "Pack{",
      iNum tag,
      iStr ",",
      iNum arity,
      iStr "}"
    ]
pprExpr (ECase condition alts) =
  iConcat
    [ iStr "case",
      iIndent (pprExpr condition),
      iStr "of",
      iNewline,
      iStr "  ",
      iIndent (iInterleave iNL (map pprAlt alts))
    ]
  where
    iNL = iStr " ;" `iAppend` iNewline
pprExpr (ELam args body) =
  iConcat
    [ iStr "\\ ",
      pprArgs args,
      iStr " . ",
      iIndent (pprExpr body)
    ]

pprAlt :: CoreAlter -> Iseqrep
pprAlt (tag, args, body) =
  iConcat
    [ iStr "<",
      iNum tag,
      iStr ">",
      iStr " -> ",
      iIndent (pprExpr body)
    ]

pprDefns :: [(Name, CoreExpr)] -> Iseqrep
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseqrep
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

--- Exercise

-- >>> pprint [("main", [], (mkMultiAp 3 (EVar "f") (EVar "x")))]
-- "main  = f x x x ;\n"

-- >>> pprint [("main", [], (mkMultiAp 3 (EVar "f") (mkMultiAp 2 (EVar "g") (EVar "x"))))]
-- "main  = f (g x x) (g x x) (g x x) ;\n"
