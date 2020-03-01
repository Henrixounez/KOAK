{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib

-- import KoakAST
import KoakPackrat
import UserInteractions

main :: IO ()
main = do
    (fileName, flag) <- handleArgs
    case fileName of
        Nothing -> handleError "You must input a .kd file\n"
        name -> do
            file <- readFile name
            case eval file of
                (Just file) -> case flag of
                    -- "-O" -> handleSoFiles
                    -- "-J" -> handleJIT
                    -- "-E" -> handleExe
                    _ -> putStrLn $ show file
                    -- False -> toLLVM $ genModule file
                    -- True -> toLLVM $ genModule file
                _ -> handleError "Error while parsing.\n"