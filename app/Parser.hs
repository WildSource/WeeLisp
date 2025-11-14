module Parser where

import Control.Monad
import Data.Char
import Types

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser i e a = Parser
  { parse :: [i] -> Either [Error i e] (a, [i])
  }

instance Functor (Parser i e) where
  fmap = liftM

instance Applicative (Parser i e) where
  pure = return
  (<*>) = ap

instance Monad (Parser i e) where
  p >>= f = Parser $ \input ->
    case parse p input of
      Left err -> Left err
      Right (x,xs) ->
        let Parser p' = f x
        in  p' xs

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \ input ->
  case input of
    [] -> Left [EndOfInput] 
    x:xs
      | predicate x -> Right (x, xs)
      | otherwise -> Left $ [Unexpected x]

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
      
