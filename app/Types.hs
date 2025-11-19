module Types where

data DataTypes
  = Number Int
  | Character Char
  | EmptyString
  | Characters String
  | Operator Char
  deriving Show
