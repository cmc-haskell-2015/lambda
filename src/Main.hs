module Main where

import Eval (parseEval)

import System.IO

-- | Считывание строки.
readLine :: IO String
readLine = putStr "> " >> hFlush stdout >> getLine

-- | Интерактивная среда программирования (REPL).
-- Завершается при вводе пустой строки.
repl :: String -> IO ()
repl [] = return ()
repl str = putStrLn (parseEval str) >> readLine >>= repl

main :: IO()
main = readLine >>= repl
