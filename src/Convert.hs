module Convert (
    stringToPoint,
    pointsToString,
    clusterToString
) where
 
import Data.List
import Types
import Utils

getValues :: String -> [String]
getValues string = words [if c == ',' || c == '(' || c == ')' then ' ' else c | c <- string]

checkFormat :: String -> Int -> Bool
checkFormat str size = case length [c | c <- str, c == ','] == size - 1 && '(' `elem` str && ')' `elem` str of
    True -> all isInt (getValues str)
    False -> False

readCoord :: String -> (Int, Int)
readCoord string = 
    let 
        values = getValues string
    in (read $ values !! 0, read $ values !! 1)

readColor :: String -> (Int, Int, Int)
readColor string = 
    let 
        values = getValues string
    in (read $ values !! 0, read $ values !! 1, read $ values !! 2)


stringToPoint :: String -> Maybe Point
stringToPoint string = do
    let wds = words string
    case length wds == 2 of
        True -> case checkFormat (wds !! 0) 2 && checkFormat (wds !! 1) 3 of
            True ->
                let
                    coord = readCoord $ wds !! 0
                    color = readColor $ wds !! 1
                in Just (Point (fst coord) (snd coord) (r color) (g color) (b color) (Cluster 0 0 0))
            False -> Nothing
        False -> Nothing

pointToString :: Point -> String
pointToString point = "(" ++ xp ++ "," ++ yp ++ ") (" ++ rp ++ "," ++ gp ++ "," ++ bp ++ ")"   
    where
        xp = show $ x point
        yp = show $ y point
        rp = show $ red point
        gp = show $ green point
        bp = show $ blue point

pointsToString :: [Point] -> String
pointsToString points = case points of
    (h:q) -> (pointsToString q) ++ (pointToString h ++ "\n")
    [] -> []

clusterToString :: Cluster -> String
clusterToString cluster = "(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"
    where
        r = show $ meanRed cluster
        g = show $ meanGreen cluster
        b = show $ meanBlue cluster