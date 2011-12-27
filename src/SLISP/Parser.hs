module SLISP.Parser (parseLisp) where
    
import SLISP.Data

import Text.ParserCombinators.Parsec
import Control.Applicative((*>), (<*))

symbol :: GenParser Char st Char
symbol = oneOf "!$%&|*+-/<=>?@^_~.`"

parseStartsWith :: Char -> (GenParser Char st E) -> (E -> E) -> GenParser Char st E
parseStartsWith c expr construct = char c >> expr >>= return . construct

parseSpaces :: GenParser Char st ()
parseSpaces =   
  let spaces' = skipMany1 (space <|> newline <|> tab) >> parseSpaces
      comment = string ";" >> skipMany (satisfy (/= '\n')) >> parseSpaces
  in (try comment) <|> spaces' <|> return ()

parseNumber :: GenParser Char st E     
parseNumber = do  
    s <- option "" $ string "-"
    n <- many1 digit
    return $ I $ read $ s ++ n

parseSymbol :: GenParser Char st E
parseSymbol = do  
    s <- symbol <|> letter
    m <- many $ symbol <|> letter <|> digit
    return $ S (s:m)

parseString :: GenParser Char st E
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"' <?> "Quote at end of string"
    return $ ST x

parseKey :: GenParser Char st E
parseKey = do
    char ':'
    S x <- parseSymbol
    parseSpaces
    y <- parseExpr
    return $ K x y

parseAtom :: GenParser Char st E
parseAtom = 
  (try parseNumber) <|> 
  (try parseSymbol) <|> 
  (try parseString) <|> 
  (try parseKey)

parseList :: GenParser Char st E
parseList = do
  char '('
  parseSpaces
  x <- parseExpr `sepEndBy` parseSpaces
  char ')' <?> "Closing bracket at end of list"
  return $ L x

parseQExpr :: GenParser Char st E
parseQExpr = parseStartsWith '\'' parseExpr Q

parseFExpr :: GenParser Char st E
parseFExpr = parseStartsWith '#' parseExpr F

parseExpr :: GenParser Char st E
parseExpr = (try parseAtom) <|> (try parseList) <|> (try parseFExpr) <|> (try parseQExpr)

parseBase :: GenParser Char st [E]
parseBase = 
  ((try parseAtom) <|> 
  (try parseList)  <|>
  (try parseQExpr) <|>
  (try parseFExpr) <|> 
  (try parseList)) `sepEndBy` parseSpaces
  
startParse :: GenParser Char st [E]
startParse = parseSpaces *> parseBase <* eof

parseLisp :: String -> [E]
parseLisp s = 
  case (parse startParse "" s) of
    Left e -> error $ show e
    Right x -> x