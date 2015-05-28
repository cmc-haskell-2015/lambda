-- |
-- BNF для лямбда-выражений:
--
-- @
-- \<строка\>        ::= :\<команда\> \<аргумент\> | \<идентификатор\> = \<уровень\> | \<уровень\>
-- \<уровень\>       ::= \<выражение\> [\<выражение\>]
-- \<выражение\>     ::= \<переменная\> | \<идентификатор\> | \<переменные\>.\<уровень\> | (\<уровень\>)
-- \<переменные\>    ::= \<переменная\> [\<переменная\>]
-- \<переменная\>    ::= \<строчная буква\> [\<строчная буква или цифра\>]
-- \<идентификатор\> ::= \<заглавная буква\> [\<строчная буква или цифра\>]
-- @
--
module Parser where

import Types
import Commands

import Data.Maybe
import Control.Applicative ((<$>))
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
        , opStart = lower
        , opLetter = alphaNum
        , reservedNames = []
        , reservedOpNames = []
        , caseSensitive = True }

-- | Парсер переменной.
parseVar :: ParserT VarName
parseVar = operator lexer

-- | Парсер списка переменных.
parseVars :: ParserT [VarName]
parseVars = many1 parseVar

-- | Парсер лямбда-выражения в скобках.
parseLambdaParen :: ParserT LambdaExpr
parseLambdaParen = parens lexer parseLevel

-- | Парсер лямбда-абстракции.
parseLambdaFunc :: ParserT LambdaExpr
parseLambdaFunc = do lexeme lexer (char '\\')
                     vars <- parseVars
                     lexeme lexer (char '.')
                     expr <- parseLevel
                     return $ foldr Lambda expr vars

-- | Поиск лямбда-терма в списке именованных лямбда-термов.
-- Возвращает ошибку парсера если в списке нет лямбда-терма с данным именем.
lookup' :: FuncName -> Maybe LambdaExpr -> ParserT LambdaExpr
lookup' name Nothing = parserFail $ name ++ " undefined"
lookup' _ (Just f) = return f
--lookup' name (Just f) = return $ Ident name

-- | Парсер подстановки именованного лямбда-терма.
parseFunCall :: ParserT LambdaExpr
parseFunCall = do name <- identifier lexer
                  st <- getState
                  lookup' name (lookup name (ftab st))

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
parseDef :: ParserT Input
parseDef = do name <- identifier lexer
              lexeme lexer (char '=')
              value <- parseLevel
              st <- getState
              return $ Def $ addFunc name value (ftab st)

-- | Парсер команды изменения порядка редукции.
setOrder :: ParserT Command
setOrder = do arg <- lexeme lexer (many1 lower)
              case arg of
                "n" -> return $ setOrder' Normal
                "a" -> return $ setOrder' Applicative
                "i" -> return $ setOrder' Interactive
                "normal" -> return $ setOrder' Normal
                "applicative" -> return $ setOrder' Applicative
                "interactive" -> return $ setOrder' Interactive
                _ -> parserFail $ "Invalid reduction order: " ++ arg

-- | Ассоциативный список парсеров команд.
commands = [("order", setOrder), ("o", setOrder)]

-- | Парсер команды интерпретатора.
parseCommand :: ParserT Input
parseCommand = do lexeme lexer (char ':')
                  name <- lexeme lexer (many1 lower)
                  let cmd = lookup name commands
                  if isNothing cmd
                  then parserFail ("Invalid command: " ++ name)
                  else liftM Cmd (fromJust cmd)

-- | Парсер строки.
lineParser :: ParserT Input
lineParser = do whiteSpace lexer
                ret <- (try parseCommand)
                   <|> (try parseDef)
                   <|> liftM Expr parseLevel
                eof
                return ret

-- | Синтаксический анализ строки.
parseLine :: Env
          -> String
          -> Either ParseError Input
parseLine st str = runParser lineParser st "<stdin>" str
