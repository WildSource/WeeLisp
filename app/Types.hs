module Types where

data DataTypes
  = Digit Char
  | Number Int
  | Character Char
  | EmptyString Char
  | Characters Char
  | Atom Char
  | Operator Char
  | LeftParenthesis Char
  | RightParenthesis Char
  deriving Show
