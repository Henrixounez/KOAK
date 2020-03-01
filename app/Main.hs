{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified LLVM.AST as AST
import Debug.Trace

import Codegen
import KoakPackrat
import PackratCleaner
import UserInteractions

initModule :: AST.Module
initModule = emptyModule "Koak Compiler"

main :: IO ()
main = do
    (fileName, trueFlag) <- handleArgs
    file <- readFile fileName
    case eval file of
        (Just file) -> case trueFlag of
            _ -> do
              codegen initModule (cleanPackrat file)
              Prelude.return ()
            -- False -> toLLVM $ genModule file
            -- True -> toLLVM $ genModule file
        _ -> handleError "Error while parsing.\n"