module Eval(parseEval) where

import Types
import Parser

import Text.Parsec.Error

eval :: Either ParseError LambdaExpr -> String
eval (Left err) = show err
eval (Right expr) = show expr

parseEval :: String -> String
parseEval str = eval $ parseLine str
