module Cluster (
    computeDistance,
    computeDistanceCluster,
    linkToCluster,
    realignClusters,
    compareClusters,
    initClusters
) where

import System.Random
import Types

computeDistance :: Point -> Point -> Float
computeDistance a b = sqrt(fromIntegral(dx + dy + dz))
    where
        dx = (red a - red b) ^ 2
        dy = (green a - green b) ^ 2
        dz = (blue a - blue b) ^ 2

computeDistanceCluster :: Point -> Cluster -> Float
computeDistanceCluster a b = computeDistance a point
    where point = Point 0 0 (meanRed b) (meanGreen b) (meanBlue b) (Cluster 0 0 0)

linkToCluster :: [Point] -> [Cluster] -> [Point]
linkToCluster points clusters = case points of
    (h:q) -> (linkToCluster q clusters) ++ [linkedPoint]
        where
            smallestDistance = minimum [computeDistanceCluster h c | c <- clusters]
            nearestCluster = [c | c <- clusters, computeDistanceCluster h c == smallestDistance] !! 0
            linkedPoint = Point (x h) (y h) (red h) (green h) (blue h) nearestCluster
    [] -> []

computeNewCluster :: [Point] -> Cluster
computeNewCluster points = Cluster r g b
    where
        ptNumber = length points
        r = div (sum [red p | p <- points]) ptNumber
        g = div (sum [green p | p <- points]) ptNumber
        b = div (sum [blue p | p <- points]) ptNumber

realignClusters :: [Point] -> [Cluster] -> [Cluster]
realignClusters points clusters = case clusters of
    (h:q) -> do
        let clusterPoints = [p | p <- points, cluster p == h]
        case null clusterPoints of
            True -> case null q of
                True -> []
                False -> realignClusters points q
            False -> (realignClusters points q) ++ [computeNewCluster clusterPoints]
    [] -> []


compareClusters :: [Cluster] -> [Cluster] -> Bool
compareClusters a b = length aElemInB == length a && length bElemInA == length b
    where
        aElemInB = [x | x <- a, x `elem` b]
        bElemInA = [x | x <- b, x `elem` a]

randomColor :: IO Int
randomColor = do
    seed <- newStdGen
    let (x, newSeed) = randomR (0, 255) seed
    setStdGen newSeed
    return x

randomCluster :: IO Cluster
randomCluster = do
    r <- randomColor
    g <- randomColor
    b <- randomColor
    return (Cluster r g b)

initClusters :: Int -> IO [Cluster]
initClusters nb = mapM (\x -> randomCluster) $ replicate nb []