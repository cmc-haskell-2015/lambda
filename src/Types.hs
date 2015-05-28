-- | Определения типов.
module Types where

import System.IO
import Control.Applicative
import Text.Parsec.Prim

import Data.Maybe

-- | Имя переменной.
type VarName = String

-- | Имя лямбда-терма.
type FuncName = String

-- | Синтаксическое дерево.
data LambdaExpr
    = Apply LambdaExpr LambdaExpr -- ^ Аппликация.
    | Lambda VarName LambdaExpr   -- ^ Лямбда-абстракция.
    | Var VarName                 -- ^ Переменная.
--    | Ident FuncName              -- ^ Именованный лямбда-терм.

tryUnchurchInt' :: VarName -> VarName -> LambdaExpr -> Maybe Int
tryUnchurchInt' x y (Apply (Var x1) expr)
    | x == x1 = (+1) <$> tryUnchurchInt' x y expr
    | otherwise = Nothing
tryUnchurchInt' x y (Var y1)
    | y == y1 = Just 0
    | otherwise = Nothing
tryUnchurchInt' _ _ _ = Nothing

tryUnchurchInt :: LambdaExpr -> Maybe Int
tryUnchurchInt (Lambda x (Lambda y expr)) = tryUnchurchInt' x y expr
tryUnchurchInt _ = Nothing

instance Show LambdaExpr where
    show (Apply f x) = '(':(show f) ++ ' ':(show x) ++ ")"
    show fun@(Lambda x f) = case tryUnchurchInt fun of
        Nothing -> "(\\" ++ x ++ '.':(show f) ++ ")"
        Just n  -> show n
    show (Var x) = x
--    show (Ident x) = x

-- | Команда интерпретатора.
type Command = Env -> Result

-- | Ассоциативный список именованных лямбда-термов.
type FuncTab = [(FuncName, LambdaExpr)]

-- | Входные данные интерпретатора.
data Input
    = Expr LambdaExpr -- ^ Лямбда-выражение.
    | Def FuncTab     -- ^ Обновленный список именованных лямбда-термов.
    | Cmd Command     -- ^ Команда интерпретатора.

-- | Порядок редукции лямбда-выражений.
data ReductionOrder
    = Normal      -- ^ Нормальный порядок редукции.
    | Applicative -- ^ Аппликативный порядок редукции.
    | Interactive -- ^ Получение всех возможных вариантов редукции.

instance Show ReductionOrder where
    show Normal = "reduction order: normal"
    show Applicative = "reduction order: applicative"
    show Interactive = "interactive reduction mode"

instance Eq ReductionOrder where
    (==) Normal Normal = True
    (==) Applicative Applicative = True
    (==) Interactive Interactive = True
    (==) _ _ = False

-- | Результат интерпретации.
data Result
    = Result
    { state :: Env
    , message :: String }

-- | Состояние среды.
data Env
    = Env
    { ftab :: FuncTab
    , order :: ReductionOrder
    , variants :: [LambdaExpr]
    , lastExpr :: LambdaExpr
    , files :: [IO Handle] }

-- | Тип парсера.
type ParserT a = Parsec String Env a

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
