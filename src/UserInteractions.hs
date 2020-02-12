module UserInteractions where

import System.Environment
import System.Exit

helper :: IO ()
helper = putStrLn "Usage: ./koak path_to_kaleidoscope source file\n"

handleError :: String -> IO a
handleError err = do
    putStrLn err
    exitWith (ExitFailure 84)


handleFlags :: [String] -> IO (Bool)
handleFlags args
    | elem "-h" args = helper >>= (\x -> exitSuccess)
    | otherwise = return False


handleArgs :: IO ((String, Bool))
handleArgs = do
    args <- getArgs
    case length args of
        0 -> handleError "You must provide a kaleidoscope source file.\n"
        _ -> do
            flagValue <- handleFlags args
            return (head args, flagValue)
