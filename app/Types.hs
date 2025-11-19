module Types where

data DataTypes
  = Digit Char
  | Number Int
  | Character Char
  | EmptyString
  | Characters String
  | Operator Char
  deriving Show
