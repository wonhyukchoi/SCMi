import Text.ParserCombinators.Parsec(oneOf, Parser, parse)
import System.Environment

main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "ERROR!!" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"
