module Main where
import System.Environment(getArgs)

main :: IO ()
-- Argument must in format [Int].
main = do
  nums <- getArgs
  let numList = map read nums :: [Int]
  putStrLn "Name?"
  name <- getLine
  putStrLn $ ("Hello " ++ name ++ "!" ++  
              "\nSum is: " ++ (show $ sum numList))