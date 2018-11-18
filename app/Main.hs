module Main where

import System.Environment (getArgs)
import System.IO
import NumConv

getBase :: String -> IO Int
getBase name = do
    putStr $ "Enter the " ++ name ++ " base: "
    base <- getLine
    case tryParse base of
        Nothing ->
            putStrLn "Not a number, try again" >> getBase name
        Just res | badBase res ->
            putStrLn "Base must be within the range [2, 36]" >> getBase name
                 | otherwise ->
            return res

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
printHelp = putStrLn $ "Usage: numConv [inputFile outputFile]/[-h]\n\
    \    When no arguments are specified, the converter is launched in the interactive mode.\n\
    \    If two files are set, the converter processes the input line per line.\n\
    \    Each line must be in the following format: `baseFrom baseTo number`\n\
    \    (for example, `2 10 1010101`)\n\
    \    -h prints this help."

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

