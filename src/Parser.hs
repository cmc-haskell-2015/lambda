module Parser(parseLine) where

import Types

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

{-
<строка> ::= <идентификатор> = <уровень> | <уровень>
<уровень> ::= <выражение> [<выражение>]
<выражение> ::= <переменная> | <идентификатор> |
                \<переменные>.<уровень> | (<уровень>)
<переменные> ::= <переменная> [<переменная>]
<переменная> ::= <строчная буква>
<идентификатор> ::= <заглавная буква> [<строчная буква | цифра>]
-}

-- | Лексический анализатор
lexer = makeTokenParser
        LanguageDef
        { commentStart = ""
        , commentEnd = ""
        , commentLine = ""
        , nestedComments = True
        , identStart = upper
        , identLetter = alphaNum
        , opStart = oneOf ""
        , opLetter = oneOf ""
        , reservedNames = []
        , reservedOpNames = []
        , caseSensitive = True }

-- | Парсер переменной
parseVar :: ParserT VarName
parseVar = lexeme lexer lower

-- | Парсер списка переменных
parseVars :: ParserT [VarName]
parseVars = many1 parseVar

-- | Парсер лямбда-выражения в скобках
parseLambdaParen :: ParserT LambdaExpr
parseLambdaParen = do lexeme lexer (char '(')
                      ret <- parseLevel
                      lexeme lexer (char ')')
                      return ret

-- | Парсер лямбда-абстракции
parseLambdaFunc :: ParserT LambdaExpr
parseLambdaFunc = do lexeme lexer (char '\\')
                     vars <- parseVars
                     lexeme lexer (char '.')
                     expr <- parseLevel
                     return $ foldr Lambda expr vars

-- | Поиск лямбда-терма в списке именованных лямбда-термов
-- | Возвращает ошибку парсера если в списке нет лямбда-терма с данным именем
lookup' :: Bool -> FuncName -> Maybe LambdaExpr -> ParserT LambdaExpr
lookup' _ name Nothing = fail $ name ++ " undefined"
lookup' True _ (Just f) = return f
lookup' False name (Just f) = return $ Ident name

-- | Парсер подстановки именованного лямбда-терма
parseFunCall :: ParserT LambdaExpr
parseFunCall = do name <- identifier lexer
                  st <- getState
                  lookup' (inRepl st) name (lookup name (ftab st))

-- | Парсер лямбда-выражения без аппликации на верхнем уровне вложенности
parseLambdaExpr :: ParserT LambdaExpr
parseLambdaExpr
     =  (try parseLambdaParen)
    <|> (try parseLambdaFunc)
    <|> liftM Var (try parseVar)
    <|> parseFunCall

-- | Парсер уровня вложенности
parseLevel :: ParserT LambdaExpr
parseLevel = (many1 parseLambdaExpr) >>= return . foldl1 Apply

-- | Возвращает ошибку парсера если список именованных лямбда-термов содержит
-- | элемент с данным именем
remove' :: Maybe LambdaExpr -> FuncName -> FuncTab -> ParserT FuncTab
remove' (Just _) name _ = fail $ "Multiple definitions of " ++ name
remove' Nothing _ ft = return ft

-- | Удаление элемента из списка именованных лямбда-термов
-- | Возвращает ошибку парсера при переопределении именованного
-- | лямбда-терма в режиме генерации кода
remove :: Bool -> FuncName -> FuncTab -> ParserT FuncTab
remove True name ft = return $ filter (not . (== name) . fst) ft
remove False name ft = remove' (lookup name ft) name ft

-- | Парсер именованного лямбда-терма
parseDef :: ParserT (Either LambdaExpr FuncTab)
parseDef = do name <- identifier lexer
              lexeme lexer (char '=')
              value <- parseLevel
              st <- getState
              ft <- remove (inRepl st) name (ftab st)
              return $ Right $ (name, value):ft

-- | Парсер строки
lambdaParser :: ParserT (Either LambdaExpr FuncTab)
lambdaParser = do whiteSpace lexer
                  ret <- (try parseDef) <|> liftM Left parseLevel
                  eof
                  return ret

-- | Синтаксический анализ строки
parseLine :: ParserState -> String -> Either ParseError (Either LambdaExpr FuncTab)
parseLine st str = runParser lambdaParser st "<stdin>" str
