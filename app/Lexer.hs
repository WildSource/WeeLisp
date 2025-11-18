module Lexer where

import Types
import Parser

digitParser :: Parser Char () DataTypes
digitParser = Parser $ \ input ->
  let
    digitParser' = satisfy isDigit 
  in
    case parse digitParser' input of
      Left err -> Left err
      Right (x,xs) -> Right (Digit x, xs)

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
      
