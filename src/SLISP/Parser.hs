module SLISP.Parser where
    
import SLISP.Data

import Text.ParserCombinators.Parsec
import Control.Applicative((*>), (<*))

symbol = oneOf "!$%&|*+-/<=>?@^_~.`"

parseStartsWith c expr construct = char c >> expr >>= return . construct

parseSpaces =   let spaces  = skipMany1 (space <|> newline <|> tab) >> parseSpaces
                    comment = string ";" >> skipMany (satisfy (/= '\n')) >> parseSpaces
                in  (try comment) <|> spaces <|> return ()
                
parseNumber = do  
    s <- option "" $ string "-"
    n <- many1 digit
    return $ I $ read $ s ++ n

parseSymbol = do  
    s <- symbol <|> letter
    m <- many $ symbol <|> letter <|> digit
    return $ S (s:m)

parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ ST x

parseKey = do
    char ':'
    S x <- parseSymbol
    parseSpaces
    y <- parseExpr
    return $ K x y

parseAtom = (try parseNumber) <|> 
            (try parseSymbol) <|> 
            (try parseString) <|> 
            (try parseKey)

parseList = do
    char '('
    parseSpaces
    x <- parseExpr `sepEndBy` parseSpaces
    char ')'
    return $ L x

parseQExpr = parseStartsWith '\'' parseExpr Q

parseFExpr = parseStartsWith '#' parseExpr F

parseExpr = (try parseAtom) <|> (try parseList) <|> (try parseFExpr) <|> (try parseQExpr)

parseBase = ((try parseAtom) <|> 
            (try parseList)  <|>
            (try parseQExpr) <|>
            (try parseFExpr) <|> 
            (try parseList)) `sepEndBy` parseSpaces
  
startParse = parseSpaces *> parseBase <* eof

parse' :: String -> [E]
parse' s = case (parse startParse "" s) of
            Left e  ->  error $ show e
            Right x -> x