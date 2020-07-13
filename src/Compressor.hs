module Compressor (
    readPoints,
    countColors,
    writeCompressed,
    core,
    compressor,
    debugClusters,
    debugPoints
) where

import Convert
import Cluster
import Types
import Utils

_readPoints :: [String] -> [Point] -> Maybe [Point]
_readPoints lns pts = case lns of
    (h:q) -> do
        let point = stringToPoint h
        case point of
            Just point -> _readPoints q (pts ++ [point])
            _ -> Nothing
    [] -> Just pts

readPoints :: [String] -> Maybe [Point]
readPoints lns = _readPoints lns []

countColors :: [Point] -> Int
countColors points = case points of
    (h:q) -> case any (\x -> red h == red x && green h == green x && blue h == blue x) q of
        True -> (countColors q) + 0
        False -> (countColors q) + 1
    [] -> 0

writeCompressed :: [Point] -> [Cluster] -> IO ()
writeCompressed points clusters = case clusters of
    (h:q) -> do
        let pointGroup = [p | p <- points, cluster p == h]
        putStrLn $ "--\n" ++ clusterToString h ++ "\n-"
        putStr $ pointsToString pointGroup
        writeCompressed points q
    _ -> putStr "\n"

core :: [Point] -> [Cluster] -> IO ()
core points clusters = do 
    writeCompressed points clusters
    let newPoints = linkToCluster points clusters
    writeCompressed newPoints clusters
    let newClusters = realignClusters newPoints clusters
    writeCompressed newPoints newClusters
    putStr "\n----------------------\n"
    case compareClusters newClusters clusters of
        True -> writeCompressed newPoints newClusters
        False -> core newPoints newClusters

compressor :: Int -> Float -> String -> IO ()
compressor colorNb convergence filepath = do
    lns <- getFileLn filepath
    let points = readPoints lns
    case points of
        Just points -> case colorNb > countColors points of
            True -> putStrLn "Number of colors in compressed picture can not be greater than initial number of colors."
            False -> do
                clusters <- initClusters colorNb
                core points clusters
        _ -> putStrLn $ "Syntax error in " ++ filepath ++ " file."

{--
debugPoints :: [Point] -> IO ()
debugPoints points = case points of
    (h:q) -> do
        print h
        debugPoints q
    [] -> putStr "\n"

debugClusters :: [Cluster] -> IO ()
debugClusters clusters = case clusters of
    (h:q) -> do
        print h
        debugClusters q
    [] -> putStr "\n"
    --}