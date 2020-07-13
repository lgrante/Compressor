module Entry where

import System.Exit
import System.Environment
import Utils
import Compressor

exitWithUsage :: IO ()
exitWithUsage = do
    putStrLn "USAGE: ./imageCompressor n e IN\n"
    putStrLn "\tn\tnumber of colors in the final image"
    putStrLn "\te\tconvergence limit"
    putStrLn "\tIN\tpath to the file containing the colors of the pixels"
    exitWith $ ExitFailure 84

entry :: IO ()
entry = do
    args <- getArgs
    case length args of
        3 -> case isInt (args !! 0) && isFloat (args !! 1) of
            True -> compressor n e filepath
                where
                    n = read $ args !! 0
                    e = read $ args !! 1
                    filepath = args !! 2
            False -> do
                print $ isInt $ args !! 0
                print $ isFloat $ args !! 1
        _ -> exitWithUsage