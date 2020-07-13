module Types where

type Color = (Int, Int, Int)

data Point = Point {
    x :: Int,
    y :: Int,
    red :: Int,
    green :: Int,
    blue :: Int,
    cluster :: Cluster
} deriving (Eq, Ord, Show, Read)

data Cluster = Cluster {
    meanRed :: Int,
    meanGreen :: Int,
    meanBlue :: Int
} deriving (Eq, Ord, Show, Read)

r (x, _, _) = x
g (_, x, _) = x
b (_, _, x) = x