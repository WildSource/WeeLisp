module Types where

data DataTypes
  = Digit Char
  | Number Int
  | Character Char
  | EmptyString
  | Characters String
  | Atom Char
  | Operator Char
  | LeftParenthesis Char
  | RightParenthesis Char
  deriving Show
