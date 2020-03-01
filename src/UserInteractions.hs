module UserInteractions where

import System.Environment
import System.Exit

import qualified Data.Text as T

helper :: IO ()
helper = putStrLn "Usage: ./koak path_to_kaleidoscope source file\n"

handleError :: String -> IO a
handleError err = do
    putStrLn err
    exitWith (ExitFailure 84)


handleKDFiles :: [String] -> IO (Maybe String)
handleKDFiles args = return $ foldr checkKd Nothing args
    where 
        separator = T.pack "."
        checkKd :: String -> Maybe String -> Maybe String
        checkKd [] acc = acc
        checkKd s (Just acc) = Just acc
        checkKd s Nothing
            | (T.unpack $ last $ T.splitOn separator (T.pack s)) == "kd" = (Just s)
            | otherwise = Nothing

handleFlags :: [String] -> IO (String)
handleFlags args
    | elem "-h" args = helper >>= (\x -> exitSuccess)
    | elem "-O" args = return "to_so"
    | elem "-J" args = return "to_jit" 
    | otherwise = return "to_exe"


handleArgs :: IO ((Maybe String, String))
handleArgs = do
    args <- getArgs
    case length args of
        0 -> handleError "You must provide a kaleidoscope source file.\n"
        _ -> do
            flagValue <- handleFlags args
            kd_input <- handleKDFiles args
            return (kd_input, flagValue)

