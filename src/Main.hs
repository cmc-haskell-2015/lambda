module Main where

import Types
import Eval

import System.IO
import Data.List
import Data.Maybe
import Text.Read

-- | Считывание строки.
readLine :: IO String
readLine = putStr "> " >> hFlush stdout >> getLine

-- | Считывание варианта редукции.
readVariant :: Int -> IO Int
readVariant len = do l <- readLine
                     let x = if null l
                             then Just (-3)
                             else readMaybe l :: Maybe Int
                     if isNothing x
                     then putStrLn (l ++ " is not an integer")
                          >> readVariant len
                     else let x' = fromJust x
                          in if x' >= len || x' < -3
                             then putStrLn ("Invalid variant: " ++ l)
                                  >> readVariant len
                             else return x'

-- | Вывод вариантов редукции.
printVariants :: Int -> [LambdaExpr] -> IO ()
printVariants x [] = putStrLn "-1) continue with applicative reduction order"
                     >> putStrLn "-2) continue with normal reduction order"
                     >> putStrLn "-3) stop"
printVariants x (y:ys) = do putStrLn $ (show x) ++ ") " ++ (show y)
                            printVariants (x + 1) ys

-- | Продолжение редукции с заданным порядком.
continue :: ReductionOrder -> Env -> IO Env
continue ord (Env ft ord' xs prev) = do putStrLn $ evalShow ord prev
                                        return (Env ft ord' [] prev)

-- | Цикл интерактивной редукции.
step' :: Env -> IO Env

step' env@(Env ft ord [] prev) = return env

step' (Env ft Interactive [x] prev)
    = do putStrLn (show x)
         step' $ Env ft Interactive (reduceAll x) x

step' env@(Env ft Interactive xs prev)
    = do printVariants 0 xs
         v <- readVariant (length xs)
         case v of
            -3 -> return env
            -2 -> continue Normal env
            -1 -> continue Applicative env
            _ -> let expr = xs !! v
                 in step' (Env ft Interactive (reduceAll expr) expr)

step' env = return env

-- | Интерактивная среда программирования (REPL).
-- Завершается при вводе пустой строки.
repl :: Env -> String -> IO ()
repl env [] = return ()
repl env str = do let res = parseEval env str
                  st <- step' (state res)
                  putStrLn $ message res
                  s <- readLine
                  repl st s

main :: IO()
main = readLine >>= (repl $ Env [] Normal [] (Var "none"))
