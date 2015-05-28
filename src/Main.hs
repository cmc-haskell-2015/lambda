module Main where

import Types
import Eval

import System.IO
import Data.List
import Data.Maybe
import Text.Read
import Control.Applicative

-- | Считывание строки из последнего открытого файла.
readLine :: Env -> IO Result
readLine env@(Env ft ord var prev hs)
    | (null hs) = return $ Result env ""
    | otherwise = do fp <- head hs
                     if (fp == stdin)
                     then putStr "> " >> hFlush stdout
                     else putStrLn "" >> hFlush stdout
                     end <- hIsEOF fp
                     if end
                     then hClose fp >> readLine (Env ft ord var prev (tail hs))
                     else hGetLine fp
                          >>= (\s -> return (s, fp))
                          >>= (\l -> Result
                               (Env ft ord var prev ((return $ snd l):(tail hs)))
                               <$> (return $ fst l))

-- | Считывание строки из stdin.
readLine' :: IO String
readLine' = putStr "> " >> hFlush stdout >> getLine

-- | Считывание варианта редукции.
readVariant :: Int -> IO Int
readVariant len = do l <- readLine'
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
continue ord (Env ft ord' xs prev hs) = do putStrLn $ evalShow ord prev
                                           return (Env ft ord' [] prev hs)

-- | Цикл интерактивной редукции.
step' :: Env -> IO Env

step' env@(Env ft ord [] prev hs) = return env

step' (Env ft Interactive [x] prev hs)
    = do putStrLn (show x)
         step' $ Env ft Interactive (reduceAll x) x hs

step' env@(Env ft Interactive xs prev hs)
    = do printVariants 0 xs
         v <- readVariant (length xs)
         case v of
            -3 -> return env
            -2 -> continue Normal env
            -1 -> continue Applicative env
            _ -> let expr = xs !! v
                 in step' (Env ft Interactive (reduceAll expr) expr hs)

step' env = return env

-- | Интерактивная среда программирования (REPL).
-- Завершается при вводе пустой строки.
repl :: Result -> IO ()
repl (Result env []) = return ()
repl (Result env str) = do let res = parseEval env str
                           st <- step' (state res)
                           putStrLn $ message res
                           readLine st >>= repl

main :: IO()
main = readLine (Env [] Normal [] (Var "none") [(return stdin)]) >>= repl
