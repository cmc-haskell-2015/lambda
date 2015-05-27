module Main where

import Types
import Eval

import System.IO
import Data.List

-- | Считывание строки.
readLine :: IO String
readLine = putStr "> " >> hFlush stdout >> getLine

-- | Вывод значения выражения или нового списка именованных лямбда-термов.
printRes :: Either String FuncTab -> IO ()
printRes (Left str) = putStrLn str
printRes (Right ft) = putStrLn $ intercalate "\n" $ map show ft

-- | Обновление списка именованных лямбда-термов.
updateFuncTab :: FuncTab -> Either String FuncTab -> FuncTab
updateFuncTab ft (Left _) = ft
updateFuncTab _ (Right ft) = ft

-- | Интерактивная среда программирования (REPL).
-- Завершается при вводе пустой строки.
repl :: FuncTab -> String -> IO ()
repl ft [] = return ()
repl ft str = do let x = parseEval ft str
                 printRes x
                 s <- readLine
                 repl (updateFuncTab ft x) s

main :: IO()
main = readLine >>= (repl [])
