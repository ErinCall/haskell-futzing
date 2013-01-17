data Section = Section { getA :: Int, getB :: Int, getC :: Int }
    deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C
    deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = (map snd pathA)
        timeB = (map snd pathB)
        forwardTimeToA = (sum timeA + a)
        crossTimeToA = (sum timeB + b + c)
        forwardTimeToB = (sum timeB + b)
        crossTimeToB = (sum timeA + a + c)
        newPathToA = if crossTimeToA > forwardTimeToA
                        then (A, a):pathA
                        else (C, c):(B, b):pathB
        newPathToB = if crossTimeToB > forwardTimeToB
                        then (B, b):pathB
                        else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath)  = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath
