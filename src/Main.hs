module Main where

import Types
import Eval

import System.IO

-- | Считывание строки
readLine :: IO String
readLine = putStr "> " >> hFlush stdout >> getLine

toString :: Either String FuncTab -> String
toString (Left str) = str
toString (Right ft) = show ft

-- | Обновление списка именованных лямбда-термов
updateFuncTab :: FuncTab -> Either String FuncTab -> FuncTab
updateFuncTab ft (Left _) = ft
updateFuncTab _ (Right ft) = ft

-- | Интерактивная среда программирования (REPL)
-- | Завершается при вводе пустой строки
repl :: FuncTab -> String -> IO ()
repl ft [] = return ()
repl ft str = do let x = parseEval ft str
                 putStrLn (toString x)
                 s <- readLine
                 repl (updateFuncTab ft x) s

main :: IO()
main = readLine >>= (repl [])
