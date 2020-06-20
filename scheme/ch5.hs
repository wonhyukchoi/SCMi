import Text.ParserCombinators.Parsec(oneOf, Parser, parse,
                                     skipMany1, space,
                                     many, noneOf, char,
                                     letter, digit, (<|>),
                                     many1,
                                     try, string,
                                     anyChar, alphaNum, notFollowedBy,
                                     sepBy, endBy, ParseError)
import System.Environment(getArgs)
import Control.Monad(liftM, mapM)
import Numeric(readOct, readHex, readFloat)
import Data.Ratio((%), Rational)
import Data.Complex(Complex, Complex((:+)))
import qualified Control.Monad.Except as E

main :: IO()
main = do
  ragu <- getArgs
  let toParse = head ragu
      evaled  = fmap show $ readExpr toParse >>= eval
      result  = extractValue $ trapError evaled
  putStrLn result

data LispError = NumArgs Integer [LispVal]
                 | TypeMisMatch String LispVal
                 | Parser ParseError
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ " : " ++ varname
showError (BadSpecialForm message form) = message ++ " : " ++ show form
showError (NotFunction message func)    = message ++ " : " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args, but found values "
                                          ++ listShower found
showError (Parser parseErr)             = "Parse error: " ++ show parseErr
showError (TypeMisMatch expected found) = "Expected type " ++ show expected
                                          ++ " but found " ++ show found

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError :: ThrowsError String -> ThrowsError String
trapError action = E.catchError action (return . show)

extractValue :: ThrowsError String -> String
extractValue (Right val) = val

{-Ch3 below-}

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "" input of
  Left err -> E.throwError $ Parser err
  Right val -> Right val

eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _) = return val
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func: args)) = mapM eval args >>= apply func
eval badForm = E.throwError $ BadSpecialForm "bad form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply "boolean?" (arg:_) = return $ Bool $ isBool arg
apply func args = maybe (err) ($ args) $ lookup func primitives
  where err = E.throwError $ NotFunction "Unrecognized primitive" func

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _ = False

-- TODO: change typing
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("number?", unaryOp numberp),
              ("symbol->string", unaryOp sym2str),
              ("string->symbol", unaryOp str2sym),
              ---Chapter 5
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))
              ]

numBoolBinop :: [LispVal] -> ThrowsError LispVal
numBoolBinop _ = Right $ Number 5

boolBoolBinop :: [LispVal] -> ThrowsError LispVal
boolBoolBinop _ = Right $ Number 5

strBoolBinop :: [LispVal] -> ThrowsError LispVal
strBoolBinop _ = Right $ Number 5

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

symbolp, numberp, sym2str, str2sym :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
sym2str (Atom a) = String a
sym2str _        = String ""
str2sym (String s) = Atom s
str2sym _          = Atom ""

numericBinop :: (Integer -> Integer -> Integer) ->
  [LispVal] -> ThrowsError LispVal
numericBinop op []            = Left   $ NumArgs 2 []
numericBinop op singleVal@[_] = Left   $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>=
                                return . Number . (foldr1 op)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
                         if null parsed
                           then E.throwError $ TypeMisMatch "number" $ String s
                           else return $ fst $ parsed !! 0
unpackNum (List [l]) = unpackNum l
unpackNum notNum = E.throwError $ TypeMisMatch "number" notNum


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
               Complex (Complex Double) deriving (Show)

-- instance Show LispVal where
--  show = showVal

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
