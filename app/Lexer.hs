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

additionParser :: Parser Char () DataTypes
additionParser = Parser $ \ input ->
  let
    operatorParser' = satisfy ('+' ==)
  in
    case parse operatorParser' input of
      Left err -> Left err
      Right (x,xs) -> Right (Operator (Addition x), xs)

subtractionParser :: Parser Char () DataTypes
subtractionParser = Parser $ \ input ->
  let
    operatorParser' = satisfy ('-' ==)
  in
    case parse operatorParser' input of
      Left err -> Left err
      Right (x,xs) -> Right (Operator (Subtraction x), xs)

multiplicationParser :: Parser Char () DataTypes
multiplicationParser = Parser $ \ input ->
  let
    operatorParser' = satisfy ('*' ==)
  in
    case parse operatorParser' input of
      Left err -> Left err
      Right (x,xs) -> Right (Operator (Multiplication x), xs)

divisionParser :: Parser Char () DataTypes
divisionParser = Parser $ \ input ->
  let
    operatorParser' = satisfy ('/' ==)
  in
    case parse operatorParser' input of
      Left err -> Left err
      Right (x,xs) -> Right (Operator (Division x), xs)

operatorParser :: Parser Char () DataTypes
operatorParser
  = additionParser
  <|> subtractionParser
  <|> multiplicationParser
  <|> divisionParser

whitespace :: Parser Char () String
whitespace = many (satisfy isSpace)

value :: Parser Char () DataTypes
value = operatorParser <|> numberParser

tokenize :: Parser Char () [DataTypes]
tokenize = some (value <* whitespace)

