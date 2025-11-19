module Lexer where

import Types
import Parser
import Data.Char

digitParser :: Parser Char () DataTypes
digitParser = Parser $ \ input -> do
  (x, xs) <- parse (satisfy isDigit) input
  Right (Digit x, xs)
  
operatorParser :: Parser Char () DataTypes
operatorParser = Parser $ \ input ->
  let operatorParser' = satisfy isOperator
  in
    case parse operatorParser' input of
      Left err -> Left err
      Right (x,xs) -> Right (Operator x, xs)
  where
    isOperator :: Char -> Bool
    isOperator x
      | x == '+' = True
      | x == '-' = True      
      | x == '*' = True
      | x == '/' = True
      | otherwise = False
      
