module UserInteractions where

import System.Environment
import System.Exit

import qualified Data.Text as T

helper :: IO ()
helper = putStrLn "Usage: ./koak (flag)+ path_to_kaleidoscope source file\n\
\FLAGS (only one at a time):\n\
\   -h | --help : display help\n\
\   -O | --so : Generate .so file\n\
\   -B | --bc : Generate .bc file\n\
\   -S | --as : Generate .s  file\n\
\   -I | --ir : Generate .ll file\n\
\   -J | --jit : Launch the JIT repl (Doesn't work with extern functions)\n\
\If no flags are given, koak will interpret (Same as -J).\n\n\
\.ll to .bc = llvm-as file.ll\n\
\.bc to .s  = llc file.s\n\
\.s  to exe = gcc file.s\n"

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
    | elem "-h" args || elem "--help" args = helper >>= (\x -> exitSuccess)
    | elem "-O" args || elem "--so" args = return "to_so"
    | elem "-B" args || elem "--bc" args = return "to_bc"
    | elem "-S" args || elem "--as" args = return "to_s"
    | elem "-I" args || elem "--ir" args = return "to_ir"
    | elem "-J" args || elem "--jit" args = return "to_jit" 
    | otherwise = return "to_jit"


handleArgs :: IO ((Maybe String, String))
handleArgs = do
    args <- getArgs
    case length args of
        0 -> handleError "You must provide a kaleidoscope source file.\n"
        _ -> do
            flagValue <- handleFlags args
            kd_input <- handleKDFiles args
            return (kd_input, flagValue)

