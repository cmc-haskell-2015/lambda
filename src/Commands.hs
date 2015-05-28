-- | Команды интерпретатора.
module Commands where

import Types

-- | Команда изменения порядка редукции.
setOrder' :: ReductionOrder -> Command
setOrder' ord (Env ft o _ l) = Result (Env ft ord [] l) (show ord)
