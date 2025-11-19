module Parsers where

import Data.Tree
import Types
import Parser
import Lexer
import Control.Applicative ((<|>))

type AST = Tree DataTypes

expressionParser :: Parser Char () [DataTypes]
expressionParser = do
  op <- operatorParser
  num1 <- (numberParser <|> numberParser)
  num2 <- (numberParser <|> numberParser)
  pure ([op, num1, num2])
