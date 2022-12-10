module Day7 (day7ex1, day7ex2, CommandLine(Cd, Ls, Dir, Size), FileStructure(Directory, File)) where

data CommandLine = Ls | Cd String | Dir String | Size Int String deriving (Show)

data FileStructure = Directory [FileStructure] Int String | File Int String deriving (Show)


day7ex1 :: [CommandLine] -> Int
day7ex1 xs = sum $ map getSize $ collectDirectoriesMatching (\x -> getSize x <= 100000) $ buildFileStructure xs

collectDirectoriesMatching :: (FileStructure -> Bool) -> FileStructure -> [FileStructure]
collectDirectoriesMatching f (Directory xs size name) = if f (Directory xs size name) then (Directory xs size name) : childDirectoriesMatching else childDirectoriesMatching
    where
        childDirectoriesMatching = concatMap (collectDirectoriesMatching f) xs
collectDirectoriesMatching _ (File _ _ ) = []

day7ex2 :: [CommandLine] -> Int
day7ex2 xs = foldr min 999999999 sizes
    where
        diskSize = 70000000
        fs = buildFileStructure xs
        rootSize = getSize fs
        freeSpace = diskSize - rootSize
        requiredSpace = 30000000 - freeSpace
        directoriesAtleastRequiredSize = collectDirectoriesMatching (\x -> getSize x >= requiredSpace) fs
        sizes = map getSize directoriesAtleastRequiredSize


buildFileStructure :: [CommandLine] -> FileStructure
buildFileStructure = buildFileStructure' [] (Directory [] 0 "/")

buildFileStructure' :: [String] -> FileStructure -> [CommandLine] -> FileStructure
buildFileStructure' _ currentStructure [] = currentStructure
buildFileStructure' (_ : currentPath) currentStructure (Cd ".." : xs) = buildFileStructure' currentPath currentStructure xs
buildFileStructure' currentPath currentStructure (Cd x : xs) = buildFileStructure' (x : currentPath) currentStructure xs
buildFileStructure' currentPath currentStructure (Ls : xs) = buildFileStructure' currentPath currentStructure xs
buildFileStructure' currentPath currentStructure ((Size s name) : xs) = buildFileStructure' currentPath (insertFile (reverse currentPath) s name currentStructure) xs
buildFileStructure' currentPath currentStructure ((Dir name) : xs) = buildFileStructure' currentPath (insertDirectory (reverse currentPath) name currentStructure) xs

insertDirectory :: [String] -> String -> FileStructure -> FileStructure
insertDirectory [x] name (Directory fs dirSize y) 
    | x == y = Directory (Directory [] 0 name : fs) dirSize y
    | otherwise = Directory fs dirSize y
insertDirectory (x : xs) name (Directory fs dirSize y)
    | null fs = Directory fs dirSize y
    | otherwise = if x == y then Directory (map (insertDirectory xs name) fs) dirSize y else Directory fs dirSize y
insertDirectory _ _ fs = fs

getSize :: FileStructure -> Int
getSize (Directory _ size _) = size
getSize (File size _) = size

insertFile :: [String] -> Int -> String -> FileStructure -> FileStructure
insertFile [] _ _ fs = fs
insertFile [x] size fileName (Directory fs dirSize y)
    | x == y = Directory (File size fileName : fs) (size + dirSize) y
    | otherwise = Directory fs dirSize y
insertFile (x : xs) size fileName (Directory fs dirSize y)
    | null fs = Directory fs dirSize y
    | otherwise = if x == y then Directory newDir newSize y else Directory fs dirSize y
                    where
                        newDir = map (insertFile xs size fileName) fs
                        newSize = (sum . map getSize) newDir
insertFile _ _ _ fs = fs
