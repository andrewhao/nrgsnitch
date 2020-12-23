module Main where


import           Lib                (parseCsv)
import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    putStrLn "The arguments are:"
    mapM_ putStrLn args
    parseCsv (args !! 0)
