{-# LANGUAGE RecordWildCards #-}

-- |
module Core1.Parser (parse, parseFile) where

import Control.Monad ((<=<))
import Data.Char (isAlpha, isDigit)
import Data.Set (Set, fromList)
import Core1.Language
    ( nonRecursive,
      recursive,
      CoreAlter,
      CoreExpr,
      CoreProgram,
      CoreScDefn,
      Expr(ELet, EAp, ELam, EVar, ENum, EConstr, ECase),
      Name )
import qualified Core1.PPrint as PPrint
import qualified Data.Bifunctor

data Token = Token {srcLineNo :: Int, tok :: String} deriving Show

-- Returns the remaining tokens to be parsed
-- Returns multiple possible parsings
type Parser a = [Token] -> [(a, [Token])]

data PartialExpr = NoOp | FoundOp Name CoreExpr

data Pos = Pos {lineNo :: Int, col :: Int}

-- Parser is made of three stages

parse :: String -> CoreProgram
parse = syntax . (uncurry clex <=< srcLines)
  where
    srcLines src = zipWith (\lineNo line -> (Pos {lineNo = lineNo, col = 0}, line)) [1 ..] $ lines src

sampleProgram :: String
sampleProgram = "f = 3; \
                \g x y = let z = x in z ; \
                \h x = case (let y = x in y) of \
                \        <1> -> 2 ; \
                \        <2> -> 5"

sampleCaseProgram :: String
sampleCaseProgram = "f x y = case x of \
                    \        <1> -> case y of \
                    \                <1> -> 1; \
                    \        <2> -> 2"

-- >>> PPrint.pprint . parse $ "f = 3; h = y"
-- "f  = 3 ;\nh  = y ;\n"

-- >>> PPrint.pprint . parse $ "f = 3 ;\nh = y"
-- "f  = 3 ;\nh  = y ;\n"

-- Ending with ; is syntax error
-- >>> PPrint.pprint . parse $ "g x y = let z = x in z;"
-- Syntax error

-- >>> PPrint.pprint . parse $ "g x y = let z = x in z"
-- "g x y = let z = x\n        in z ;\n"

-- >>> PPrint.pprint . parse $ sampleProgram
-- "f  = 3 ;\ng x y = let z = x\n        in z ;\nh x = case let y = x\n           in y of\n        <1> -> 2 ;\n        <2> -> 5 ;\n"

-- >>> PPrint.pprint . parse $ sampleCaseProgram
-- "f x y = case x of\n          <1> -> case y of\n                   <1> -> 1 ;\n          <2> -> 2 ;\n"

-- >>>PPrint.pprint . parse $ "f x = (id) (x) (1) ((id) (x))"
-- "f x = id x 1 (id x) ;\n"


-- 1. Read source code from input file
parseFile :: FilePath -> IO CoreProgram
parseFile filename = do
  contents <- readFile filename
  return $ parse contents


-- 2. Core lexer (one line)
clex :: Pos -> String -> [Token]
clex _ [] = []
clex Pos {col = 0} ('|' : '|' : cs) = [] -- ignore comment line
clex Pos {col = 0} cs
  | all isWhiteSpace cs = [] -- ignore whitespace line
clex p@Pos {..} (c1 : c2 : cs)
  | [c1, c2] `elem` twoCharOps = Token {srcLineNo = lineNo, tok = [c1, c2]} : clex p {col = col + 2} cs
clex p@Pos {..} (c : cs)
  | isWhiteSpace c = clex p {col = col + 1} cs
clex p@Pos {..} (c : cs)
  | isDigit c = Token {srcLineNo = lineNo, tok = numToken} : clex p {col = col + tokSize} rest
  where
    numToken = c : takeWhile isDigit cs
    tokSize = length numToken
    rest = dropWhile isDigit cs
clex p@Pos {..} (c : cs)
  | isAlpha c = Token {srcLineNo = lineNo, tok = varTok} : clex p {col = col + tokSize} rest
  where
    varTok = c : takeWhile isIdChar cs
    tokSize = length varTok
    rest = dropWhile isIdChar cs
clex p@Pos {..} (c : cs) =
  Token {srcLineNo = lineNo, tok = [c]} : clex p {col = col + 1} cs

-- For debugging lexer output
debugLexer :: String -> [Token]
debugLexer = uncurry clex <=< srcLines
  where
    srcLines src = zipWith (\lineNo line -> (Pos {lineNo = lineNo, col = 0}, line)) [1 ..] $ lines src

-- >>> debugLexer $ "f = 3"
-- [Token {srcLineNo = 1, tok = "f"},Token {srcLineNo = 1, tok = "="},Token {srcLineNo = 1, tok = "3"}]

-- >>> debugLexer $ "f = 3 ;\nh = y"
-- [Token {srcLineNo = 1, tok = "f"},Token {srcLineNo = 1, tok = "="},Token {srcLineNo = 1, tok = "3"},Token {srcLineNo = 1, tok = ";"},Token {srcLineNo = 2, tok = "h"},Token {srcLineNo = 2, tok = "="},Token {srcLineNo = 2, tok = "y"}]


-- 3. syntax analysis
syntax :: [Token] -> CoreProgram
syntax = takeFirstCompleteParse . pProgram

takeFirstCompleteParse :: [(p, [Token])] -> p
takeFirstCompleteParse ((prog, []) : others) = prog
takeFirstCompleteParse (parse : others) = takeFirstCompleteParse others
takeFirstCompleteParse _ = error "Syntax error"


pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")


-- >>> takeFirstCompleteParse . pProgram $ debugLexer "f = 3; y = 4"
-- [("f",[],ENum 3),("y",[],ENum 4)]

-- All parse attempts
-- >>> pProgram $ debugLexer "f = 3; y = 4"
-- [([("f",[],ENum 3),("y",[],ENum 4)],[]),([("f",[],ENum 3)],[Token {srcLineNo = 1, tok = ";"},Token {srcLineNo = 1, tok = "y"},Token {srcLineNo = 1, tok = "="},Token {srcLineNo = 1, tok = "4"}])]

-- >>> takeFirstCompleteParse . pProgram $ debugLexer "f = 3 ;\nh = y"
-- [("f",[],ENum 3),("h",[],EVar "y")]

-- >>> syntax $ debugLexer "f = 3 ;\nh = y"
-- [("f",[],ENum 3),("h",[],EVar "y")]

-- >>> syntax $ debugLexer $ "f = 3 ;\nh = y"
-- [("f",[],ENum 3),("h",[],EVar "y")]


-- super combinator parser
pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mkSc var1 vars _ expr = (var1, vars, expr)

pExpr :: Parser CoreExpr
pExpr =
  pLetExpr `pAlt`
  pCaseExpr `pAlt`
  pLamExpr `pAlt`
  pExpr1


-- Infix operator and function application parsing challenges:
-- Challenge #1: Operator precedence and associativity
--   We will creating a parsing rules that is precdence-aware/associativity-aware
--   expr1 with lowest precedence.

-- expr1 -> expr2 | expr1         -- high precedence, right-associative
--        | expr2                 -- fallback case (no | operator)
-- expr2 -> expr3 & expr2
--        | expr3
-- expr3 -> expr4 relop expr4      -- non-associative
--        | expr4
-- expr4 -> expr5 + expr4         -- right-associative
--        | expr5 - expr5         -- Why not expr4? non-associative
--        | expr5
-- expr5 -> expr6 * expr5         -- right-associative
--        | expr6 / expr6         -- non-associative
--        | expr6
-- expr6 -> aexpr1 ... aexprn     -- function application

-- Challenge #2: Efficiency.
-- As seen above the fallback case (where there is no | operator), it is very inefficient.
-- In `expr2 | expr1` first parse `expr2` and then check for `|`, but if there is no `|`,
-- then parse `expr2` once again. As it goes up the precedence levels,
-- the redundant parsings become quadratic.

-- We can solve the redundant parsing, by changing our grammar as following.
-- expr1  -> expr2 expr1c
-- expr1c -> | expr1
--         | empty


-- >>> takeFirstCompleteParse . pExpr1 $ debugLexer $ "f x y"
-- EAp (EAp (EVar "f") (EVar "x")) (EVar "y")

-- >>> debugLexer $ "x & y"
-- [Token {srcLineNo = 1, tok = "x"},Token {srcLineNo = 1, tok = "&"},Token {srcLineNo = 1, tok = "y"}]

-- >>> takeFirstCompleteParse . pExpr1 $ debugLexer $ "x | y"
-- EAp (EAp (EVar "/") (EVar "x")) (EVar "y")

-- >>> takeFirstCompleteParse . pExpr1 $ debugLexer $ "x + 3 * y"
-- EAp (EAp (EVar "+") (EVar "x")) (EAp (EAp (EVar "*") (ENum 3)) (EVar "y"))

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr1 :: Parser CoreExpr
pExpr1 =
  pThen assembleOp pExpr2 pExpr1c
  where
    pExpr1c :: Parser PartialExpr
    pExpr1c = pThen FoundOp (pLit "|") pExpr1 `pAlt` pEmpty NoOp

pExpr2 :: Parser CoreExpr
pExpr2 =
  pThen assembleOp pExpr3 pExpr2c
  where
    pExpr2c :: Parser PartialExpr
    pExpr2c = pThen FoundOp (pLit "&") pExpr2 `pAlt` pEmpty NoOp

relOps :: Set String
relOps = fromList ["<", "<=", "==", "~=", ">=", ">"]

pExpr3 :: Parser CoreExpr
pExpr3 =
  pThen3 mkAp pExpr4 (pSat (`elem` relOps)) pExpr4 `pAlt`
  pExpr4
  where
    mkAp a f b = EAp (EAp (EVar f) a) b

pExpr4 :: Parser CoreExpr
pExpr4 =
  pThen assembleOp pExpr5 pExpr4c `pAlt`             -- right-associative
  pThen3 mkMinus pExpr5 (pLit "-") pExpr5            -- non-associative
  where
    pExpr4c :: Parser PartialExpr
    pExpr4c = pThen FoundOp (pLit "+") pExpr4 `pAlt` pEmpty NoOp

    mkMinus a _ b = EAp (EAp (EVar "-") a) b

pExpr5 :: Parser CoreExpr
pExpr5 =
  pThen assembleOp pExpr6 pExpr5c `pAlt`             -- right-associative
  pThen3 mkDiv pExpr6 (pLit "/") pExpr6              -- non-associative
  where
    pExpr5c :: Parser PartialExpr
    pExpr5c = pThen FoundOp (pLit "*") pExpr5 `pAlt` pEmpty NoOp

    mkDiv a _ b = EAp (EAp (EVar "/") a) b

pExpr6 :: Parser CoreExpr
pExpr6 = pApExpr `pAlt` pAExpr

-- Ordinary {expr -> expr aexpr} is not parsable due to left-recursion
-- Thus we confine our grammar to: {expr -> aexpr ... aexpr }
-- (both function and arguments needs to wrapped in paranthesis)
pApExpr :: Parser CoreExpr
pApExpr = pThen combine pAExpr (pOneOrMore pAExpr)
  where
    combine f args = mkAp' (EAp f) args

    mkAp' _ [] = error "Impossible happened"
    mkAp' acc [a] = acc a
    mkAp' acc (a : as) = mkAp' (EAp (acc a)) as

-- Until lambda lifting is implemented, we can't do anything with lambdas.
pLamExpr :: Parser CoreExpr
pLamExpr = pThen4 mkLam (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
  where
    mkLam _ vars _ body = ELam vars body

-- Atomic expression
pAExpr :: Parser CoreExpr
pAExpr =
  pApply pVar EVar `pAlt`
  pApply pNum ENum `pAlt`
  pConstr `pAlt`
  pThen3 (\_ e _ -> e) (pLit "(") pExpr (pLit ")")

pConstr :: Parser CoreExpr
pConstr = pThen6 mkConstr (pLit "Pack") (pLit "{") pNum (pLit ",") pNum (pLit "}")
  where
    mkConstr _ _ constTagNo _ constArity _ = EConstr constTagNo constArity

pCaseExpr :: Parser CoreExpr
pCaseExpr = pThen4 mkCase (pLit "case") pExpr (pLit "of") pAlters
  where
    mkCase _ caseExpr _ caseAlters = ECase caseExpr caseAlters

    pAlters :: Parser [CoreAlter]
    pAlters = pOneOrMoreWithSep pAlter (pLit ";")

    pAlter :: Parser CoreAlter
    pAlter = pThen6 mkAlter (pLit "<") pNum (pLit ">") (pZeroOrMore pVar) (pLit "->") pExpr

    mkAlter _ consTagNo _ consArgs _ altBody = (consTagNo, consArgs, altBody)

pLetExpr :: Parser CoreExpr
pLetExpr = pLet `pAlt` pLetRec
  where
    pLet = pThen4 mkLet (pLit "let") pDefns (pLit "in") pExpr
    mkLet _ defns _ bodyExpr = ELet nonRecursive defns bodyExpr

    pLetRec = pThen4 mkLetRec (pLit "letrec") pDefns (pLit "in") pExpr
    mkLetRec _ defns _ bodyExpr = ELet recursive defns bodyExpr

    pDefns :: Parser [(String, CoreExpr)]
    pDefns = pOneOrMoreWithSep pDefn (pLit ";")

    pDefn :: Parser (String, CoreExpr)
    pDefn = pThen3 (\var _ varExpr -> (var, varExpr)) pVar (pLit "=") pExpr

-- helpers

twoCharOps :: Set String
twoCharOps = fromList ["==", " ̃=", ">=", "<=", "->"]

isWhiteSpace :: Char -> Bool
isWhiteSpace = (`elem` " \t\n")

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

-- parsers combinators

pEmpty :: a -> Parser a
pEmpty a toks = [(a, toks)]

-- Run two parsers.
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = do
  (a, rest1) <- p1 toks
  (b, rest2) <- p2 rest1
  return (combine a b, rest2)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks = do
  (a, rest1) <- p1 toks
  (b, rest2) <- p2 rest1
  (c, rest3) <- p3 rest2
  return (combine a b c, rest3)

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks = do
  (a, rest1) <- p1 toks
  (b, rest2) <- p2 rest1
  (c, rest3) <- p3 rest2
  (d, rest4) <- p4 rest3
  return (combine a b c d, rest4)

pThen5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
pThen5 combine p1 p2 p3 p4 p5 toks = do
  (a, rest1) <- p1 toks
  (b, rest2) <- p2 rest1
  (c, rest3) <- p3 rest2
  (d, rest4) <- p4 rest3
  (e, rest5) <- p5 rest4
  return (combine a b c d e, rest5)

pThen6 :: (a -> b -> c -> d -> e -> f -> g)
  -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
pThen6 combine p1 p2 p3 p4 p5 p6 toks = do
  (a, rest1) <- p1 toks
  (b, rest2) <- p2 rest1
  (c, rest3) <- p3 rest2
  (d, rest4) <- p4 rest3
  (e, rest5) <- p5 rest4
  (f, rest6) <- p6 rest5
  return (combine a b c d e f, rest6)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p1 = pOneOrMore p1 `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p1 = pThen combine p1 (pZeroOrMore p1)
  where combine = (:)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p1 f toks = Data.Bifunctor.first f <$> p1 toks

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 pSep = pThen3 (\p _ ps -> p : ps) p1 pSep (pOneOrMoreWithSep p1 pSep) `pAlt` pApply p1 (:[])

-- parser with satisfy predicate
pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat predicate (Token {..} : rest)
  | predicate tok = [(tok, rest)]
  | otherwise = []

-- Does it match given string?
pLit :: String -> Parser String
pLit s = pSat (== s)

keywords :: Set String
keywords = fromList ["let", "letrec", "case", "in", "of", "Pack"]

pVar :: Parser String
pVar = pSat (every (isAlpha . head, all isIdChar, not . (`elem` keywords)))
  where every (af, bf, cf) x = af x && bf x && cf x

-- >>> pVar $ debugLexer "someVar someVar2"
-- [("someVar",[Token {srcLineNo = 1, tok = "someVar2"}])]

-- >>> pVar $ debugLexer "2someVar"
-- []

-- >>> pVar $ debugLexer "letrec someVar someVar2"
-- []


pNum :: Parser Int
pNum = pApply (pSat (all isDigit)) read

-- >>> pNum $ debugLexer "1234"
-- [(1234,[])]

-- >>> pNum $ debugLexer "someVar"
-- []

-- >>> pNum $ debugLexer "2someVar"
-- [(2,[Token {srcLineNo = 1, tok = "someVar"}])]
