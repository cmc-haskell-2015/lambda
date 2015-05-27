-- | Интерпретатор.
module Eval (parseEval) where

import Types
import Parser

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Parsec.Error

-- | Функция, определяющая, содержит ли лямбда-выражение
-- свободную переменную с данным именем.
contains :: VarName -> LambdaExpr -> Bool

contains x (Var y) = x == y

contains x (Lambda y expr) = (x /= y) && contains x expr

contains x (Apply ex1 ex2) = contains x ex1 || contains x ex2

-- | Попытка альфа-конверсии.
tryAlphaConvert :: VarName -> VarName -> LambdaExpr -> Maybe LambdaExpr

tryAlphaConvert x x1 var@(Var y) | (y == x) = Just $ Var x1
                                 | (y == x1) = Nothing
                                 | otherwise = Just var

tryAlphaConvert x x1 fun@(Lambda y expr)
    | (y == x1) = if contains x expr then Nothing else Just fun
    | (y == x) = if contains x1 expr then Nothing else Just fun
    | otherwise = Lambda y <$> tryAlphaConvert x x1 expr

tryAlphaConvert x x1 (Apply ex1 ex2) = Apply
                                       <$> tryAlphaConvert x x1 ex1
                                       <*> tryAlphaConvert x x1 ex2

-- | Альфа-конверсия лямбда-абстракции.
alphaConvert :: LambdaExpr -> LambdaExpr -> Int -> LambdaExpr

alphaConvert fun@(Lambda x ex) arg n
    | (contains x1 arg || isNothing conv) = alphaConvert fun arg (n + 1)
    | otherwise = Lambda x1 $ fromJust conv
    where x1 = x ++ (show n)
          conv = tryAlphaConvert x x1 ex

alphaConvert _ _ _ = error "alphaConvert"

-- | Подстановка выражения-аргумента вместо связанной переменной
-- после альфа-конверсии.
replaceArg' :: VarName -> LambdaExpr -> LambdaExpr -> LambdaExpr
replaceArg' x arg fun@(Lambda y ex) = Lambda y $ replaceArg x arg ex
replaceArg' _ _ _ = error "replaceArg'"

-- | Подстановка выражения-аргумента вместо связанной переменной.
replaceArg :: VarName -> LambdaExpr -> LambdaExpr -> LambdaExpr

replaceArg x arg var@(Var y) | (x == y) = arg
                             | otherwise = var

replaceArg x arg fun@(Lambda y ex)
    | (x == y) = fun
    | (contains y arg) = replaceArg' x arg $ alphaConvert fun arg 0
    | otherwise = Lambda y $ replaceArg x arg ex

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
eval :: Either ParseError (Either LambdaExpr FuncTab) -> Either String FuncTab

eval (Left err) = Left $ show err

eval (Right (Left expr))
    = Left
    $ intercalate "\n"
    $ map show
    $ reverse
    $ eval' expr

eval (Right (Right ft)) = Right ft

-- | Синтаксический анализ и интерпретация лямбда-выражения.
parseEval :: FuncTab -> String -> Either String FuncTab
parseEval ft str
    = eval
    $ parseLine ParserState { ftab = ft } str
