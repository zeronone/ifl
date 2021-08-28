module Main where

import Core1.Parser (parseFile)
import Core1.PPrint (pprint)
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  let filename = head args

  putStrLn "Parsed AST"
  prog <- parseFile filename
  print prog

  putStrLn "\n\nPretty printed version"
  putStrLn "|----------------"
  putStrLn . pprint $ prog
  putStrLn "|----------------"
