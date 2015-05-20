-- | 
-- BNF для лямбда-выражений:
--
-- @
-- \<уровень\>    ::= \<выражение\> [\<выражение\>]
-- \<выражение\>  ::= \<переменная\> | \<переменные\>.\<уровень\> | (\<уровень\>)
-- \<переменные\> ::= \<переменная\> [\<переменная\>]
-- \<переменная\> ::= \<строчная буква\>
-- @
--
module Parser where

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
parseVar :: Parser VarName
parseVar = lexeme lexer lower

-- | Парсер списка переменных.
parseVars :: Parser [VarName]
parseVars = many1 parseVar

-- | Парсер лямбда-выражения в скобках.
parseLambdaParen :: Parser LambdaExpr
parseLambdaParen = do lexeme lexer (char '(')
                      ret <- parseLevel
                      lexeme lexer (char ')')
                      return ret

-- | Парсер лямбда-абстракции.
parseLambdaFunc :: Parser LambdaExpr
parseLambdaFunc = do lexeme lexer (char '\\')
                     vars <- parseVars
                     lexeme lexer (char '.')
                     expr <- parseLevel
                     return $ foldr Lambda expr vars

-- | Парсер лямбда-выражения без аппликации на верхнем уровне вложенности.
parseLambdaExpr :: Parser LambdaExpr
parseLambdaExpr
     =  (try parseLambdaParen)
    <|> (try parseLambdaFunc)
    <|> liftM Var parseVar

-- | Парсер уровня вложенности.
parseLevel :: Parser LambdaExpr
parseLevel = (many1 parseLambdaExpr) >>= return . foldl1 Apply

-- | Парсер строки.
lambdaParser :: Parser LambdaExpr
lambdaParser = do whiteSpace lexer
                  ret <- parseLevel
                  eof
                  return ret

-- | Синтаксический анализ строки.
parseLine :: String -> Either ParseError LambdaExpr
parseLine str = parse lambdaParser "<stdin>" str
