module Lib
    ( someFunc
    ,div'
    ) where

import           Data.List
import           Data.Maybe



someFunc :: IO ()
someFunc = putStrLn "someFunc"

div' :: Int -> Int -> Int
div' a b = a `div` b


safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just $ a `div` b
