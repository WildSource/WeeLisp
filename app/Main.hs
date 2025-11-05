module Main (main) where

import System.Environment (getArgs)

readSourceCode :: [String] -> IO String
readSourceCode (x:_) = readFile x
readSourceCode [] = pure ""

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- readSourceCode args
  let codeLines = lines sourceCode
  print codeLines
  
