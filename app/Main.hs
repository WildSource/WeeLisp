module Main (main) where

import Control.Monad
import System.Environment (getArgs)
import WeeLisp

readSourceCode :: [String] -> IO String
readSourceCode (x:_) = readFile x
readSourceCode [] = pure ""

interpret :: [String] -> IO ()
interpret args = do
  sourceCode <- readSourceCode args
  interpret' sourceCode

interpret' :: String -> IO ()
interpret' sourceCode = do
  let tokens = parse tokenize sourceCode

  case tokens of
    Left err -> print err
    Right (tokens', _) -> do
      let ast = parse expressionParser tokens'
      case ast of
        Left err -> print err
        Right ast' -> eval $ fst ast'

repl :: IO ()
repl = do
  codeLine <- getLine
  interpret' codeLine

main :: IO ()
main = do
  args <- getArgs

  if null args
  then forever repl
  else interpret args
  
