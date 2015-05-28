-- | Команды интерпретатора.
module Commands where

import Types

import System.IO

-- | Команда изменения порядка редукции.
setOrder' :: ReductionOrder -> Command
setOrder' ord (Env ft o _ l hs) = Result (Env ft ord [] l hs) (show ord)

-- | Команда загрузки программы из файла.
loadFile' :: String -> Command
loadFile' fname (Env ft ord vars prev hs)
    = Result (Env ft ord vars prev ((openFile fname ReadMode):hs)) ""
