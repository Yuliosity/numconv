module NumConv where

import Data.Char (chr, ord)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Numeric (readInt, showIntAtBase)

runReadS :: Read a => ReadS a -> String -> Maybe a
runReadS parser str = case parser str of
    [(res, "")] -> Just res
    _           -> Nothing

tryParse :: Read a => String -> Maybe a
tryParse = runReadS reads

badBase :: Int -> Bool
badBase base = base < 2 || base > 36

convertFrom :: Int -> String -> Maybe Int
convertFrom base str
    | badBase base = Nothing
    | "-" `isPrefixOf` str = Nothing
    | otherwise = runReadS (readInt base isValid (fromJust . toDigit)) str where
    --isValid c = toLower c `elem` take base ['0'..'9'] ++ take (base - 10) ['a'..'z']
    isValid c = case toDigit c of
        Nothing -> False
        Just d  -> d < base

    toDigit c | '0' <= c && c <= '9' = Just $ ord c - ord '0'
              | 'a' <= c && c <= 'z' = Just $ ord c - ord 'a' + 10
              | 'A' <= c && c <= 'Z' = Just $ ord c - ord 'A' + 10
              | otherwise = Nothing

convertTo :: Int -> Int -> Maybe String
convertTo base num
    | badBase base = Nothing
    | num < 0 = Nothing
    | otherwise = Just $ showIntAtBase base toDigit num "" where
    toDigit d | d <= 9 = chr $ ord '0' + d
              | otherwise = chr $ ord 'a' + d - 10

convertFromTo :: Int -> Int -> String -> Maybe String
convertFromTo from to str = do
    num <- convertFrom from str
    convertTo to num
