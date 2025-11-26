module Main (main) where

import System.Environment (getArgs)
import WeeLisp

readSourceCode :: [String] -> IO String
readSourceCode (x:_) = readFile x
readSourceCode [] = pure ""

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- readSourceCode args

  let tokens = parse tokenize sourceCode

  case tokens of
    Left err -> print err
    Right (tokens', _) -> do
      let ast = parse expressionParser tokens'
      case ast of
        Left err -> print err
        Right ast' -> print $ fst ast'
  
