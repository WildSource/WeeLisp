module Parser where

import Data.Bool
import Data.Char
import Types

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }

digitParser :: Parser Char () (DataTypes Char)
digitParser = Parser $ \ input ->
  case input of
    (x:xs) -> bool (Left ([Unexpected x])) (Right (Digit x, xs)) (isDigit x) 
    [] -> Left $ [Empty]

operatorParser :: Parser Char () (DataTypes Char)
operatorParser = Parser $ \ input ->
  case input of
    (x:xs) -> bool (Left ([Unexpected x])) (Right (Operator x, xs)) (isOperator x)
    [] -> Left $ [Empty]
  where
    isOperator :: Char -> Bool
    isOperator x
      | x == '+' = True
      | x == '-' = True      
      | x == '*' = True
      | x == '/' = True
      | otherwise = False
