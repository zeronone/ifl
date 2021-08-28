-- |

module Core1.Examples where

import Core1.Language
    ( CoreProgram, Expr(EVar, ENum, ELet, EAp), nonRecursive )


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
