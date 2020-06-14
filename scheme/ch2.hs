import Text.ParserCombinators.Parsec(oneOf, Parser, parse,
                                     skipMany1, space,
                                     many, noneOf, char,
                                     letter, digit, (<|>),
                                     many1,
                                     try, string,
                                     anyChar, alphaNum, notFollowedBy,
                                     sepBy, endBy)
import System.Environment(getArgs)
import Control.Monad(liftM)
import Numeric(readOct, readHex, readFloat)
import Data.Ratio((%), Rational)
import Data.Complex(Complex, Complex((:+)))

-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

main = do
  (expr:_) <- getArgs
  putStrLn $ readTest expr

-- Recursive Parsers
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- Return values

-- Exercises

readTest :: String -> String
readTest input = case parse parseExpr "" input of
  Left err -> "Failure! " ++ "\n" ++ show err
  Right val -> "Sucess! " ++ show val

parseNumberEx1a :: Parser LispVal
parseNumberEx1a = do
  nums <- many1(digit)
  let result = Number $ read nums
  return result

parseNumberEx1b :: Parser LispVal
parseNumberEx1b =
  many1(digit) >>= return . Number . read

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf("\\\"")
  return x

parseStringEx2 :: Parser LispVal
parseStringEx2 = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

escapedChars3 :: Parser Char
escapedChars3 = do
  char '\\'
  x <- oneOf("\\\"nrt")
  return $ case x of
    '\\' -> x
    '"' -> x
    't' -> '\t'
    'n' -> '\n'
    'r' -> '\r'

parseStringEx3 :: Parser LispVal
parseStringEx3 = do
  char '"'
  x <- many $ escapedChars3 <|> noneOf "\""
  char '"'
  return $ String x

parseNumberEx4Bases :: Parser LispVal
parseNumberEx4Bases = do
  char '#'
  base <- oneOf "ox"
  val <- many1(digit <|> letter)
  return $ case base of
    'o' -> Number . fst . head $ readOct val
    'x' -> Number . fst . head $ readHex val

parseNumberEx4 :: Parser LispVal
parseNumberEx4 = parseDecimal1
                 <|> parseNumberEx4Bases

parseChar :: Parser LispVal
parseChar = do
  try $ string "#\\"
  val <- try (string "space" <|> string "newline")
        <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
  return $ Character $ case val of
    "space" -> ' '
    "newline" -> '\n'
    x -> head x


parseFloat :: Parser LispVal
parseFloat = do
  base <- many1 digit
  char '.'
  fractional <- many1 digit
  let floatVal = read base ++ "." ++ read fractional
      takeFloat = fst . head . readFloat
  (return . Float . takeFloat) floatVal

parseRatio :: Parser LispVal
parseRatio = do
  numerator <- many1 digit
  char '/'
  denominator <- many1 digit
  let ratioVal = (read numerator) % (read denominator)
  return $ Ratio ratioVal

parseComplex:: Parser LispVal
parseComplex = do
  realPart <- many1(digit)
  char '+'
  imagPart <- many1(digit)
  char 'i'
  let complexVal = read realPart :+ read imagPart
  return $ Complex complexVal

-- Content

readExpr :: String -> String
readExpr input = case parse parseExpr msg input of
  Left err -> input ++ msg
  Right val -> "Found value: " ++ show val
  where msg = " cannot be parsed"

parseDecimal1 :: Parser LispVal
parseDecimal1 = liftM (Number . read) $ many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  x <- many1(digit)
  return . Number . read $ x

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1(oneOf "10")
  (return . Number . takeBinHelper) x

takeBinHelper :: String -> Integer
takeBinHelper = takeBin 0

takeBin :: Integer -> String -> Integer
takeBin prevVal "" = prevVal
takeBin prevVal (x:xs) =
  let newVal = read [x]
      accVal = prevVal * 2 + newVal
  in takeBin accVal xs

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1(oneOf "0123456789abcdef")
  (return . Number . takeHex) x
  where takeHex = fst . head . readHex


parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1(oneOf "01234567")
  (return . Number . takeOct) x
  where takeOct = fst . head . readOct


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  second <- many (letter <|> digit <|> symbol)
  let atom = first:second
  return $ case atom of
    _    -> Atom atom

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseComplex
            <|> parseFloat
            <|> try parseRatio
            <|> parseNumber
            <|> parseBool
            <|> parseChar
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> try parseDottedList
                   char ')'
                   return x

parseNumber :: Parser LispVal
parseNumber = parseDecimal1
              <|> parseDecimal2
              <|> parseHex
              <|> parseOct
              <|> parseBin

data LispVal = Atom String |
               List [LispVal] |
               DottedList [LispVal] LispVal |
               Number Integer |
               String String |
               Bool Bool |
               Character Char |
               Float Double |
               Ratio Rational |
               Complex (Complex Double)  deriving (Show)

-- Whitespace

spaces :: Parser ()
spaces = skipMany1 space

readExpr2 :: String -> String
readExpr2 input = case parse (spaces >> symbol) " BAD" input of
  Left err -> "error!" ++ show err
  Right val -> "Found value" ++ [val]

-- Writing a simple parser

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr1 :: String -> String
readExpr1 input = case parse symbol "ERROR!!" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"
