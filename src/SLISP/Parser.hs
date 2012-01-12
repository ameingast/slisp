module SLISP.Parser(
  parseLisp
) where
    
import SLISP.Data

import Text.ParserCombinators.Parsec
import Control.Applicative((*>), (<*), (<$))
import Control.Monad(liftM, liftM2)

symbol :: GenParser Char st Char
symbol = oneOf "!$%&|*+-/<=>?@^_~.`"

parseStartsWith :: Char -> (GenParser Char st Expression) -> (Expression -> Expression) -> GenParser Char st Expression
parseStartsWith c expr construct = char c >> expr >>= return . construct

parseSpaces :: GenParser Char st ()
parseSpaces =   
  let spaces' = skipMany1 (space <|> newline <|> tab) >> parseSpaces
      comment = string ";" >> skipMany (satisfy (/= '\n')) >> parseSpaces
  in (try comment) <|> spaces' <|> return ()

parseSignum :: GenParser Char st String
parseSignum = ("" <$ string "+") <|> string "-" <|> string ""

parseFixnum :: GenParser Char st Expression
parseFixnum = 
  liftM (Fixnum . read) $ liftM2 (++) parseSignum (many1 digit)

parseFloat :: GenParser Char st Expression
parseFloat = do
  s <- parseSignum
  pre <- many1 digit
  pt <- char '.'
  pst <- many1 digit <?> "Floatnum digits after comma"
  return $ Floatnum $ read $ s ++ pre ++ [pt] ++ pst

parseSymbolString :: GenParser Char st String
parseSymbolString = 
  liftM2 (:) (symbol <|> letter) (many $ symbol <|> letter <|> digit)

parseSymbol :: GenParser Char st Expression
parseSymbol = liftM Symbol parseSymbolString

parseString :: GenParser Char st Expression
parseString = 
  liftM Str $ (string "\"") *> (many (noneOf "\"")) <* (string "\"")

parseKey :: GenParser Char st Expression
parseKey = liftM Key $ (string ":") *> parseSymbolString

parseMap :: GenParser Char st Expression
parseMap = liftM2 Map (parseKey <* parseSpaces) parseExpr

parseAtom :: GenParser Char st Expression
parseAtom = 
  (try parseFloat) <|>
  (try parseFixnum)  <|>
  parseSymbol <|> 
  parseString <|>
  (try parseMap) <|>
  parseKey

parseList :: GenParser Char st Expression
parseList = do
  char '('
  parseSpaces
  x <- parseExpr `sepEndBy` parseSpaces
  char ')' <?> "Closing bracket at end of list"
  return $ List x

parseQExpr :: GenParser Char st Expression
parseQExpr = parseStartsWith '\'' parseExpr Quote

parseFExpr :: GenParser Char st Expression
parseFExpr = parseStartsWith '#' parseExpr Function

parseExpr :: GenParser Char st Expression
parseExpr = 
  parseAtom  <|> 
  parseList  <|> 
  parseFExpr <|> 
  parseQExpr

parseBase :: GenParser Char st [Expression]
parseBase = 
  (parseAtom <|> 
  parseList  <|>
  parseQExpr <|>
  parseFExpr) `sepEndBy` parseSpaces
  
startParse :: GenParser Char st [Expression]
startParse = parseSpaces *> parseBase <* eof

parseLisp :: String -> [Expression]
parseLisp s = 
  case (parse startParse "" s) of
    Left e -> error $ show e
    Right x -> x