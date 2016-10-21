data Section = Section {getA:: Int, getB:: Int, getC:: Int} deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label,Int)]

roadStep :: (Path,Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
         let pathAPrice = sum $ map snd pathA
             pathBPrice = sum $ map snd pathB
             directToA = pathAPrice + a
             crossToA = pathBPrice + c + b  
             directToB = pathBPrice + b
             crossToB = pathAPrice + c + a
             optimalToA = if (directToA < crossToA)
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
             optimalToB = if (directToB < crossToB)
                        then (B,b):pathB
                        else (C,c):(A,a):pathA  
         in
             (optimalToA, optimalToB)

groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = []
groupOf n xs = take n xs : groupOf n (drop n xs)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
            let
                (pathA, pathB) = foldl roadStep ([], []) roadSystem
                pathAPrice = sum $ map snd pathA
                pathBPrice = sum $ map snd pathB
            in
                if (pathAPrice < pathBPrice)
                then reverse pathA
                else reverse pathB

main = do
    contents <- getContents
    let roadSystem = map (\[a,b,c] -> Section a b c) $ groupOf 3 $ map read $ lines contents
        path = optimalPath roadSystem
        pathString = foldl (++) "" $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice
         
     
     
