-- | Определения типов.
module Types where

import System.IO
import Control.Applicative
import Text.Parsec.Prim

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

instance Show LambdaExpr where
    show (Apply f x) = '(':(show f) ++ ' ':(show x) ++ ")"
    show (Lambda x f) = "(\\" ++ x ++ '.':(show f) ++ ")"
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
