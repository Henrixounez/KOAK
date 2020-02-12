module Main where

import Lib

import KoakPackrat
import UserInteractions

main :: IO ()
main = do
    (fileName, trueFlag) <- handleArgs
    file <- readFile fileName
    case eval file of
        (Just file) -> case trueFlag of
            False -> putStrLn $ show file
            True -> putStrLn $ show file
        _ -> handleError "Error while parsing.\n"