module Types where

data DataTypes
  = Number Int
  | Character Char
  | EmptyString
  | Characters String
  | Operator Operator
  deriving Show

data Operator
  = Addition Char
  | Subtraction Char
  | Multiplication Char
  | Division Char
  deriving Show
