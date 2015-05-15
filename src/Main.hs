module Main where

import Eval

import System.IO

-- Интерактивная среда программирования (REPL)
-- Завершается при вводе пустой строки

-- | Вычисление и вывод значения выражения
evalPrint :: String -> IO ()
evalPrint [] = return ()
evalPrint str = putStrLn (parseEval str) >> readLine

-- | Считывание выражения
readLine :: IO ()
readLine = putStr "> " >> hFlush stdout >> getLine >>= evalPrint

main = readLine
