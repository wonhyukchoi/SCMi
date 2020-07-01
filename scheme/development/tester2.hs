import System.IO as IO
import System.Environment(getArgs)

main = do
    args <- getArgs
    print $ length args
    print $ null args
