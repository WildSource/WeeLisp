module Lexer where

import Types
import Parser
import Data.Char
import Control.Applicative

numberParser :: Parser Char () DataTypes
numberParser = do
  numStr <- some rawDigitParser
  let num = read numStr :: Int
  pure (Number num)
  where
    rawDigitParser :: Parser Char () Char
    rawDigitParser = satisfy isDigit

emptyStringParser :: Parser Char () DataTypes
emptyStringParser = do
  _ <- string "\"\""
  pure EmptyString

charactersParser :: Parser Char () DataTypes
charactersParser = do
  chars <- some (satisfy isAlpha)
  pure (Characters chars)

operatorParser :: Parser Char () DataTypes
operatorParser = Parser $ \ input ->
  let
    operatorParser' = satisfy isOperator
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

whitespace :: Parser Char () String
whitespace = many (satisfy isSpace)

value :: Parser Char () DataTypes
value = operatorParser <|> numberParser

tokenize :: Parser Char () [DataTypes]
tokenize = some (value <* whitespace)

