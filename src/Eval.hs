-- | ???
module Eval where

import Types
import Parser

import Data.List
import Control.Applicative
import Control.Monad
import Text.Parsec.Error

-- | Функция, определяющая, содержит ли лямбда-выражение
-- свободную переменную с данным именем.
contains :: VarName -> LambdaExpr -> Bool

contains x (Var y) = x == y

contains x (Lambda y expr) = (x /= y) && contains x expr

contains x (Apply ex1 ex2) = contains x ex1 || contains x ex2

-- | Подстановка выражения-аргумента вместо связанной переменной.
replaceArg :: VarName -> LambdaExpr -> LambdaExpr -> LambdaExpr

replaceArg x arg var@(Var y) | (x == y) = arg
                             | otherwise = var

replaceArg x arg fun@(Lambda y ex) | (x == y) = fun
                                   | otherwise = Lambda y (replaceArg x arg ex)

replaceArg x arg (Apply ex1 ex2) = Apply (replaceArg x arg ex1)
                                         (replaceArg x arg ex2)

-- | Эта-конверсия.
etaReduce :: LambdaExpr -> LambdaExpr

etaReduce
    (Lambda x (Apply expr (Var y)))
            | (x == y && (not $ contains x expr)) = etaReduce expr
            | otherwise = Lambda x (Apply (etaReduce expr) (Var y))

etaReduce (Lambda x expr) = Lambda x (etaReduce expr)
etaReduce (Apply ex1 ex2) = Apply (etaReduce ex1) (etaReduce ex2)
etaReduce expr = expr

-- | Выбор выражения для бета-редукции.
reduce'' :: Eval LambdaExpr -> LambdaExpr -> Eval LambdaExpr
reduce'' (Cont ex1) ex2 = pure $ Apply ex1 ex2
reduce'' (Stop ex1) ex2 = Apply ex1 <$> reduce' ex2

-- | Бета-редукция (нормальный порядок редукции).
reduce' :: LambdaExpr -> Eval LambdaExpr
reduce' (Apply (Lambda x func) expr) = pure $ replaceArg x expr func
reduce' (Apply ex@(Apply ex0 ex1) ex2) = reduce'' (reduce' ex) ex2
reduce' (Apply ex1 ex2) = Apply ex1 <$> reduce' ex2
reduce' (Lambda x expr) = Lambda x <$> reduce' expr
reduce' expr = Stop expr

-- | Цикл редукции.
-- Возвращает список шагов редукции.
eval'' :: Eval LambdaExpr -> [LambdaExpr] -> [LambdaExpr]
eval'' (Stop expr) xs = (etaReduce expr):xs
eval'' (Cont expr) xs = eval'' (reduce' expr) $ expr:xs

-- | Интерпретация лямбда-выражения.
-- Возвращает список шагов редукции.
eval' :: LambdaExpr -> [LambdaExpr]
eval' expr = eval'' (pure expr) []

-- | Интерпретация синтаксического дерева или ошибки парсера.
eval :: Either ParseError LambdaExpr -> String
eval (Left err) = show err
eval (Right expr) = intercalate "\n" $ map show $ reverse $ eval' expr

-- | Синтаксический анализ и интерпретация лямбда-выражения.
parseEval :: String -> String
parseEval str = eval $ parseLine str
