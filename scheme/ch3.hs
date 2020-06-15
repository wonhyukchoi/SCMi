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

main :: IO()
main = do
  getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "" input of
  Left err -> String $ "Failure! " ++ "\n" ++ show err
  Right val -> val

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Bool _) = val
eval val@(Number _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func: args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
                         if null parsed then 0
                         else fst $ parsed !! 0
unpackNum (List [l]) = unpackNum l
unpackNum _ = 0


-- oldMain = do
--   (expr:_) <- getArgs
--   putStrLn $ readExpr expr

parseTest :: Parser LispVal
parseTest = parseNumber

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseRatio
            <|> parseNumber
            <|> parseBool
            <|> parseChar
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> try parseDottedList
                   char ')'
                   return x

data LispVal = Atom String |
               List [LispVal] |
               DottedList [LispVal] LispVal |
               Number Integer |
               String String |
               Bool Bool |
               Character Char |
               Float Double |
               Ratio Rational |
               Complex (Complex Double)
{-
Ch3
-}

showVal :: LispVal -> String
showVal (Atom contents) = contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number value) = show value
showVal (Float value) = show value
showVal (Ratio value) = show value
showVal (Character value) = show value
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ listShower contents ++ ")"
showVal (DottedList listPart elemPart) = "("
                                         ++ listShower listPart
                                         ++ "." ++ showVal elemPart
                                         ++ ")"

listShower :: [LispVal] -> String
listShower = unwords . (map showVal)

instance Show LispVal where
  show = showVal

{-
Imports from ch2
-}

parseNumber :: Parser LispVal
parseNumber = parseDecimal1
              <|> parseDecimal2
              <|> parseHex
              <|> parseOct
              <|> parseBin

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

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"
