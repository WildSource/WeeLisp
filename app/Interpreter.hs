module Interpreter where

import Parsers
import Data.Graph
import Types

eval :: AST -> IO ()
eval (Node (Operator operator) [Node (Number a) _, Node (Number b) _]) = do
  putStr "WeeLisp :> "
  case operator of
    Addition '+' -> print $ a + b
    Subtraction '-' -> print $ a - b
    Multiplication '*' -> print $ a * b
    Division '/' -> print $ (fromIntegral a / fromIntegral b :: Double)
    _ -> putStrLn "SegFault lol"
eval _ = putStrLn "SegFault lol"
  
