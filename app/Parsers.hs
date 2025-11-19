module Parsers where

import Data.Tree
import Types(DataTypes (..))
import Parser(satisfy, Error (..), Parser (parse))
import Lexer
import Control.Applicative ((<|>))

type AST = Tree DataTypes
type LexerOutput = (Either [Error Char ()] (DataTypes, [Char]))

isOperator :: DataTypes -> Bool
isOperator (Operator _) = True
isOperator _ = False

isNumber :: DataTypes -> Bool
isNumber (Number _) = True
isNumber _ = False

isValue :: DataTypes -> Bool
isValue x = isOperator x || isNumber x

expressionParser :: Parser DataTypes () AST
expressionParser = do
  op <- satisfy isOperator
  arg1 <- satisfy isValue
  arg2 <- satisfy isValue
  let ast = Node op [Node arg1 [], Node arg2 []]
  pure ast

generateAST :: String -> AST
generateAST input =
  case parse tokenize input of
    Right (tokens, _) -> parse expressionParser tokens
    Left err -> error $ show err

