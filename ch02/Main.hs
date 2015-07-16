{-#LANGUAGE FlexibleContexts#-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escaped <|> noneOf "\""
    char '"'
    return $ String x
  where escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
        escapedChar code replacement = char code >> return replacement
        codes =        ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

--1.1 Rewrite parseNumber, without liftM, using do-notation
parseNumber' :: Parser LispVal
parseNumber' = do
  val <- many1 digit
  return $ Number (read val)

--1.2 Rewrite parseNumber, without liftM, using explicit sequencing with the
--  (>>=) operator
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

