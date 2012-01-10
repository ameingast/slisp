module SLISP.Parser(
  parseLisp
) where
    
import SLISP.Data

import Text.ParserCombinators.Parsec
import Control.Applicative((*>), (<*), (<$))
import Control.Monad(liftM, liftM2)

symbol :: GenParser Char st Char
symbol = oneOf "!$%&|*+-/<=>?@^_~.`"

parseStartsWith :: Char -> (GenParser Char st E) -> (E -> E) -> GenParser Char st E
parseStartsWith c expr construct = char c >> expr >>= return . construct

parseSpaces :: GenParser Char st ()
parseSpaces =   
  let spaces' = skipMany1 (space <|> newline <|> tab) >> parseSpaces
      comment = string ";" >> skipMany (satisfy (/= '\n')) >> parseSpaces
  in (try comment) <|> spaces' <|> return ()

parseSignum :: GenParser Char st String
parseSignum = ("" <$ string "+") <|> string "-" <|> string ""

parseFixnum :: GenParser Char st E     
parseFixnum = do  
  s <- parseSignum
  n <- many1 digit <?> "Fixnum digits"
  return $ I $ read $ s ++ n

parseFloat :: GenParser Char st E
parseFloat = do
  s <- parseSignum
  pre <- many1 digit
  pt <- char '.'
  pst <- many1 digit <?> "Floatnum digits after comma"
  return $ Fl $ read $ s ++ pre ++ [pt] ++ pst
  
parseSymbol :: GenParser Char st E
parseSymbol = 
  liftM2 (\a b -> S (a:b)) 
    (symbol <|> letter) 
    (many $ symbol <|> letter <|> digit)

parseString :: GenParser Char st E
parseString =
  liftM ST $ (string "\"") *> (many (noneOf "\"")) <* (string "\"")

parseKey :: GenParser Char st E
parseKey = do
  char ':'
  S x <- parseSymbol
  parseSpaces
  y <- parseExpr
  return $ K x y

parseAtom :: GenParser Char st E
parseAtom = 
  (try parseFloat) <|>
  (try parseFixnum)  <|>
  parseSymbol <|> 
  parseString <|> 
  parseKey

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
parseExpr = 
  parseAtom  <|> 
  parseList  <|> 
  parseFExpr <|> 
  parseQExpr

parseBase :: GenParser Char st [E]
parseBase = 
  (parseAtom <|> 
  parseList  <|>
  parseQExpr <|>
  parseFExpr) `sepEndBy` parseSpaces
  
startParse :: GenParser Char st [E]
startParse = parseSpaces *> parseBase <* eof

parseLisp :: String -> [E]
parseLisp s = 
  case (parse startParse "" s) of
    Left e -> error $ show e
    Right x -> x