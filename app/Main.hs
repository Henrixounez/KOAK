{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import KoakPackrat
import UserInteractions
import Entrypoint

main :: IO ()
main = do
    (fileName, flag) <- handleArgs
    case fileName of
        Nothing -> handleError "You must provide a .kd file"
        (Just name) -> do
            file <- readFile name
            case eval file of
                (Right koakAST) -> entrypoint koakAST (name, flag)
                (Left (Just s)) -> handleError s
                (Left Nothing) -> handleError "Error while parsing.\n"
