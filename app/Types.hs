module Types where

data DataTypes a
  = Digit a
  | Number a
  | Char a
  | EmptyString a
  | String a
  | Atom a
  | Operator a
  deriving Show
