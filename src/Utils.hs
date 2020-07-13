module Utils (
    isInt,
    isFloat,
    getFileLn,
) where

isInt :: String -> Bool
isInt string = length filtered == length string
    where filtered = [x | x <- string, x >= '0', x <= '9']

isFloat :: String -> Bool
isFloat string = length filtered == length string
    where filtered = [x | x <- string, ((x >= '0' && x <= '9') || x == '.')]

getFileLn :: FilePath -> IO [String]
getFileLn filepath = do
    file <- readFile filepath
    return $ lines file