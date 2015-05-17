module Types where

import Control.Applicative

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

-- | Состояние интерпретатора
data Eval a
    = Cont a -- состояние продолжения интерпретации
    | Stop a -- состояние остановки

instance Functor Eval where
    fmap f x = (pure f) <*> x

instance Applicative Eval where
    pure = Cont
    Cont f <*> Stop expr = Stop (f expr)
    Cont f <*> Cont expr = Cont (f expr)
    Stop f <*> Stop expr = Stop (f expr)
    Stop f <*> Cont expr = Stop (f expr)
{-
instance Monad Eval where
    return = pure
    Cont expr >>= f = f expr
    Stop expr >>= f = Stop expr
-}
