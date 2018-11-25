module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO
import NumConv

getBase :: String -> IO Int
getBase name = do
    putStr $ "Enter the " ++ name ++ " base: "
    base <- getLine
    case tryParse base of
        Nothing -> putStrLn "Not a number, try again" >> getBase name
        Just res | badBase res -> putStrLn "Base must be within the range [2, 36]" >> getBase name
                 | otherwise   -> return res

runInteractive :: IO ()
runInteractive = do
    hSetBuffering stdout NoBuffering
    from <- getBase "input"
    to <- getBase "output"
    putStr "Enter the number: "
    num <- getLine
    putStrLn $ fromMaybe "Bad number" $ convertFromTo from to num

processLine :: String -> String
processLine str = case words str of
    [baseFrom, baseTo, num] -> case (tryParse baseFrom, tryParse baseTo) of
        (Just from, Just to) -> fromMaybe "Bad number" $ convertFromTo from to num
        (Nothing, _) -> "Bad input base"
        (_, Nothing) -> "Bad output base"
    _ -> "Wrong format"

printHelp :: IO ()
printHelp = putStrLn "Usage:\n\
    \numConv\n\
    \    When no arguments are specified, the converter is launched in the interactive mode\n\
    \    and asks for the input and output bases and the number to be converted from the\n\
    \    command line.\n\
    \numConv inputFile outputFile\n\
    \    If two files are set, the converter processes the inputFile line per line and writes\n\
    \    results in the outputFile. Each input line must be in the following format:\n\
    \    `baseFrom baseTo number` (for example, `2 10 1010101`). If an error during the\n\
    \    line processing is occured, the error message is written instead.\n\
    \numConv -h\n\
    \    Prints this help.\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runInteractive
        ["-h"] -> printHelp
        [fileFrom, fileTo] -> do
            content <- readFile fileFrom
            writeFile fileTo $ unlines $ map processLine $ lines content
        _ -> putStrLn "Bad arguments combination; try run with -h"

