{-# LANGUAGE TypeApplications #-}

-- |
module Core1.PPrint (pprint) where

import Core1.Language
  ( CoreAlter,
    CoreExpr,
    CoreProgram,
    CoreScDefn,
    Expr (EAp, ECase, EConstr, ELam, ELet, ENum, EVar),
    Name,
    isAtomicExpr,
  )
import Data.Set (Set, fromList, member)

pprint :: CoreProgram -> String
pprint prog = iDisplay @Iseqrep (pprProgram prog)

pprProgram :: (Iseq iseq) => CoreProgram -> iseq
pprProgram scdefns =
  iInterleave iNewline $ map pprScDefn scdefns
  where
    pprScDefn :: (Iseq iseq) => CoreScDefn -> iseq
    pprScDefn (name, args, expr) =
      iConcat
        [ iStr name,
          iSpace,
          pprArgs args,
          iStr " = ",
          iIndent (pprExpr expr),
          iStr " ;"
        ]

-- Pretty Print instructions
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
    where
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

iSpace :: Iseq iseq => iseq
iSpace = iStr " "

pprArgs :: (Iseq iseq) => [Name] -> iseq
pprArgs args = iInterleave iSpace (map iStr args)

pprAExpr :: (Iseq iseq) => CoreExpr -> iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

infixOperators :: Set Name
infixOperators = fromList ["+", "-", "*", "/", "==", "~=", ">", ">=", "<", "<=", "&", "|"]

pprExpr :: (Iseq iseq) => CoreExpr -> iseq
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp (EAp (EVar op) e1) e2)
  | op `member` infixOperators =
    iConcat [pprAExpr e1, iSpace, iStr op, iSpace, pprAExpr e2]
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
    keyword :: String
    keyword = if isrec then "letrec" else "let"

    pprDefns :: (Iseq iseq) => [(Name, CoreExpr)] -> iseq
    pprDefns defns = iInterleave sep (map pprDefn defns)

    sep :: (Iseq iseq) => iseq
    sep = iConcat [iStr ";", iNewline]

    pprDefn :: (Iseq iseq) => (Name, CoreExpr) -> iseq
    pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]
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
      iSpace,
      iIndent (pprExpr condition),
      iSpace,
      iStr "of",
      iNewline,
      iStr "  ",
      iIndent (iInterleave iNL (map pprAlt alts))
    ]
  where
    iNL :: (Iseq iseq) => iseq
    iNL = iStr " ;" `iAppend` iNewline

    pprAlt :: (Iseq iseq) => CoreAlter -> iseq
    pprAlt (tag, args, body) =
      iConcat
        [ iStr "<",
          iNum tag,
          iStr ">",
          iStr " -> ",
          iIndent (pprExpr body)
        ]
pprExpr (ELam args body) =
  iConcat
    [ iStr "\\ ",
      pprArgs args,
      iStr " . ",
      iIndent (pprExpr body)
    ]

-- helper function for replicating arguments with currying
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (replicate n e2)

--- Exercise

-- >>> pprint [("main", [], (mkMultiAp 3 (EVar "f") (EVar "x")))]
-- "main  = f x x x ;\n"

-- >>> pprint [("main", [], (mkMultiAp 3 (EVar "f") (mkMultiAp 2 (EVar "g") (EVar "x"))))]
-- "main  = f (g x x) (g x x) (g x x) ;\n"
