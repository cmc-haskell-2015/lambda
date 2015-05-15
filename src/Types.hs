module Types where

-- | Имя переменной
type VarName = Char

-- | Синтаксическое дерево
data LambdaExpr
    = Apply LambdaExpr LambdaExpr -- аппликация
    | Lambda VarName LambdaExpr   -- лямбда-абстракция
    | Var VarName                 -- переменная

instance Show LambdaExpr where
    show (Apply f x) = '(':(show f) ++ ' ':(show x) ++ ")"
    show (Lambda x f) = "(\\" ++ x:'.':(show f) ++ ")"
    show (Var x) = [x]
