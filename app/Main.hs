{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib

import KoakAST
import KoakPackrat
import UserInteractions

main :: IO ()
main = do
    (fileName, trueFlag) <- handleArgs
    file <- readFile fileName
    case eval file of
        (Just file) -> case trueFlag of
            False -> toLLVM $ genModule file
            True -> toLLVM $ genModule file
        _ -> handleError "Error while parsing.\n"