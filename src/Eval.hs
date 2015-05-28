-- | Интерпретатор.
module Eval where

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

{-
-- | Подстановка выражения-аргумента вместо связанной переменной
-- после альфа-конверсии.
replaceArg' :: VarName -> LambdaExpr -> LambdaExpr -> LambdaExpr
replaceArg' x arg fun@(Lambda y ex) = Lambda y $ replaceArg x arg ex
replaceArg' _ _ _ = error "replaceArg'"
-}

-- | Подстановка выражения-аргумента вместо связанной переменной.
replaceArg :: VarName -> LambdaExpr -> LambdaExpr -> LambdaExpr

replaceArg x arg var@(Var y) | (x == y) = arg
                             | otherwise = var

replaceArg x arg fun@(Lambda y ex)
    | (x == y) = fun
    | (contains y arg) = replaceArg x arg $ alphaConvert fun arg 0
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

-- | Выбор выражения для бета-редукции (нормальный порядок редукции).
reduceNormal' :: Eval LambdaExpr -> LambdaExpr -> Eval LambdaExpr
reduceNormal' (Cont ex1) ex2 = pure $ Apply ex1 ex2
reduceNormal' (Stop ex1) ex2 = Apply ex1 <$> reduceNormal ex2

-- | Бета-редукция (нормальный порядок редукции).
reduceNormal :: LambdaExpr -> Eval LambdaExpr

reduceNormal (Apply (Lambda x func) expr) = pure $ replaceArg x expr func

reduceNormal (Apply ex@(Apply ex0 ex1) ex2)
    = reduceNormal' (reduceNormal ex) ex2

reduceNormal (Apply ex1 ex2) = Apply ex1 <$> reduceNormal ex2

reduceNormal (Lambda x expr) = Lambda x <$> reduceNormal expr

reduceNormal expr = Stop expr

-- | Бета-редукция аппликации лямбда-абстракции
-- (аппликативный порядок редукции).
reduceApplicative'' :: Eval LambdaExpr -> LambdaExpr -> Eval LambdaExpr
reduceApplicative'' (Stop (Lambda x func)) expr = pure $ replaceArg x expr func
reduceApplicative'' (Cont func) expr = pure $ Apply func expr
reduceApplicative'' _ _ = error "reduceApplicative''"

-- | Выбор выражения для бета-редукции (аппликативный порядок редукции).
reduceApplicative' :: LambdaExpr -> Eval LambdaExpr -> Eval LambdaExpr

reduceApplicative' ex1 (Cont ex2) = pure $ Apply ex1 ex2

reduceApplicative' (Lambda x func) (Stop expr)
    = reduceApplicative'' (Lambda x <$> (reduceApplicative func)) expr

reduceApplicative' ex1 (Stop ex2) = Apply
                                    <$> reduceApplicative ex1
                                    <*> pure ex2

-- | Бета-редукция (аппликативный порядок редукции).
reduceApplicative :: LambdaExpr -> Eval LambdaExpr

reduceApplicative (Apply ex1 ex2)
    = reduceApplicative' ex1 (reduceApplicative ex2)

reduceApplicative (Lambda x expr) = Lambda x <$> reduceApplicative expr

reduceApplicative expr = Stop expr

-- | Получение всех вариантов редукции.
reduceAll :: LambdaExpr -> [LambdaExpr]

reduceAll (Apply (Lambda x func) expr)
    = (replaceArg x expr func)
    : (map (Apply (Lambda x func)) (reduceAll expr))
    ++ (map (\e -> (Apply (Lambda x e) expr)) (reduceAll func))

reduceAll (Apply ex1 ex2)
    = (map (Apply ex1) (reduceAll ex2))
    ++ (map (flip Apply ex2) (reduceAll ex1))

reduceAll (Lambda x func)
    = map (Lambda x) (reduceAll func)

reduceAll (Var x) = []

-- | Бета-редукция.
reduce' :: ReductionOrder -> LambdaExpr -> Eval LambdaExpr
reduce' Normal = reduceNormal
reduce' Applicative = reduceApplicative
reduce' Interactive = error "reduce' Interactive"

-- | Цикл редукции.
-- Возвращает список шагов редукции.
eval'' :: ReductionOrder -> Eval LambdaExpr -> [LambdaExpr] -> [LambdaExpr]
eval'' _ (Stop expr) xs = (etaReduce expr):xs
eval'' ord (Cont expr) xs = eval'' ord (reduce' ord expr) $ expr:xs

-- | Интерпретация лямбда-выражения.
-- Возвращает список шагов редукции.
eval' :: ReductionOrder -> LambdaExpr -> [LambdaExpr]
eval' ord expr = eval'' ord (pure expr) []

-- | Интерпретация лямбда-выражения.
-- Возвращает список шагов редукции в виде строки.
evalShow :: ReductionOrder -> LambdaExpr -> String
evalShow ord expr = intercalate "\n"
                  $ map show
                  $ reverse
                  $ eval' ord expr

-- | Интерпретация синтаксического дерева или ошибки парсера.
eval :: Env -> Either ParseError Input -> Result

eval env (Left err) = Result env $ show err

eval env@(Env ft ord var prev hs) (Right (Expr expr))
    | (ord == Interactive) = Result (Env ft ord (reduceAll expr) expr hs) ""
    | otherwise = Result (Env ft ord var expr hs) (evalShow (order env) expr)

eval env (Right (Def ft)) = Result
                            (Env ft (order env) [] (lastExpr env) (files env))
                            (intercalate "\n" $ map show ft)

eval env (Right (Cmd func)) = func env

-- | Синтаксический анализ и интерпретация лямбда-выражения.
parseEval :: Env -> String -> Result
parseEval env str = eval env $ parseLine env str
