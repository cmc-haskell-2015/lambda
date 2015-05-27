-- | Определения типов.
module Types where

import Control.Applicative
import Text.Parsec.Prim

-- | Имя переменной.
type VarName = Char

-- | Имя лямбда-терма.
type FuncName = String

-- | Синтаксическое дерево.
data LambdaExpr
    = Apply LambdaExpr LambdaExpr -- ^ Аппликация.
    | Lambda VarName LambdaExpr   -- ^ Лямбда-абстракция.
    | Var VarName                 -- ^ Переменная.
    | Ident FuncName              -- ^ Именованный лямбда-терм.

instance Show LambdaExpr where
    show (Apply f x) = '(':(show f) ++ ' ':(show x) ++ ")"
    show (Lambda x f) = "(\\" ++ x:'.':(show f) ++ ")"
    show (Var x) = [x]
    show (Ident x) = x

-- | Ассоциативный список именованных лямбда-термов.
type FuncTab = [(FuncName, LambdaExpr)]

-- | Состояние парсера.
data ParserState
    = ParserState
    { ftab :: FuncTab
    , static :: Bool }

-- | Тип парсера.
type ParserT a = Parsec String ParserState a

-- | Состояние интерпретатора.
data Eval a
    = Cont a -- ^ Состояние продолжения интерпретации.
    | Stop a -- ^ Состояние остановки.

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
