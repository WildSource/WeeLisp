module Parser where

import Control.Monad
import Control.Applicative (Alternative(..), (<|>))
import Data.List

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
  pure x = Parser $ \ input ->
    Right (x, input)
  (<*>) = ap

instance Monad (Parser i e) where
  Parser p >>= f = Parser $ \input -> do
    (output, rest) <- p input
    parse (f output) rest

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \ input ->
  case input of
    [] -> Left [EndOfInput] 
    x:xs
      | predicate x -> Right (x, xs)
      | otherwise -> Left $ [Unexpected x]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)
