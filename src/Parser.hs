-- |
-- BNF для лямбда-выражений:
--
-- @
-- \<строка\>        ::= \<идентификатор\> = \<уровень\> | \<уровень\>
-- \<уровень\>       ::= \<выражение\> [\<выражение\>]
-- \<выражение\>     ::= \<переменная\> | \<идентификатор\> | \<переменные\>.\<уровень\> | (\<уровень\>)
-- \<переменные\>    ::= \<переменная\> [\<переменная\>]
-- \<переменная\>    ::= \<строчная буква\>
-- \<идентификатор\> ::= \<заглавная буква\> [\<строчная буква или цифра\>]
-- @
--
module Parser (parseLine) where

import Types

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

-- | Лексический анализатор.
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

-- | Парсер переменной.
parseVar :: ParserT VarName
parseVar = lexeme lexer lower

-- | Парсер списка переменных.
parseVars :: ParserT [VarName]
parseVars = many1 parseVar

-- | Парсер лямбда-выражения в скобках.
parseLambdaParen :: ParserT LambdaExpr
parseLambdaParen = do lexeme lexer (char '(')
                      ret <- parseLevel
                      lexeme lexer (char ')')
                      return ret

-- | Парсер лямбда-абстракции.
parseLambdaFunc :: ParserT LambdaExpr
parseLambdaFunc = do lexeme lexer (char '\\')
                     vars <- parseVars
                     lexeme lexer (char '.')
                     expr <- parseLevel
                     return $ foldr Lambda expr vars

-- | Поиск лямбда-терма в списке именованных лямбда-термов.
-- Возвращает ошибку парсера если в списке нет лямбда-терма с данным именем.
lookup' :: Bool -> FuncName -> Maybe LambdaExpr -> ParserT LambdaExpr
lookup' _ name Nothing = fail $ name ++ " undefined"
lookup' True _ (Just f) = return f
lookup' False name (Just f) = return $ Ident name

-- | Парсер подстановки именованного лямбда-терма.
parseFunCall :: ParserT LambdaExpr
parseFunCall = do name <- identifier lexer
                  st <- getState
                  lookup' (static st) name (lookup name (ftab st))

-- | Парсер лямбда-выражения без аппликации на верхнем уровне вложенности.
parseLambdaExpr :: ParserT LambdaExpr
parseLambdaExpr
     =  (try parseLambdaParen)
    <|> (try parseLambdaFunc)
    <|> liftM Var (try parseVar)
    <|> parseFunCall

-- | Парсер уровня вложенности.
parseLevel :: ParserT LambdaExpr
parseLevel = (many1 parseLambdaExpr) >>= return . foldl1 Apply

-- | Добавление элемента в список именованных лямбда-термов.
addFunc :: FuncName -> LambdaExpr -> FuncTab -> FuncTab
addFunc name fun ft = (name, fun):(filter (not . (== name) . fst) ft)

-- | Парсер именованного лямбда-терма.
parseDef :: ParserT (Either LambdaExpr FuncTab)
parseDef = do name <- identifier lexer
              lexeme lexer (char '=')
              value <- parseLevel
              st <- getState
              return $ Right $ addFunc name value (ftab st)

-- | Парсер строки.
lineParser :: ParserT (Either LambdaExpr FuncTab)
lineParser = do whiteSpace lexer
                ret <- (try parseDef) <|> liftM Left parseLevel
                (try endOfLine >> return ()) <|> eof
                return ret

-- | Синтаксический анализ строки.
parseLine :: ParserState
          -> String
          -> Either ParseError (Either LambdaExpr FuncTab)
parseLine st str = runParser lineParser st "<stdin>" str
