module Main where

import System.IO
import System.Directory
import Data.List (isInfixOf, sort)
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Map as M
import SumNonEmpty (sumNonEmpty)
import Utils (printTitle, readIntList, safeReadInt)

-- HC13T1: List Files in Directory
listFiles :: IO [FilePath]
listFiles = listDirectory "."

-- HC13T2: Filter Files by Substring
filterFilesBySubstring :: String -> IO [FilePath]
filterFilesBySubstring sub = do
    filter (isInfixOf sub) <$> listFiles

-- HC13T3: Sort and Return Filtered Files
sortFilteredFiles :: String -> IO [FilePath]
sortFilteredFiles sub = do
    filtered <- filterFilesBySubstring sub
    return (sort filtered)

-- HC13T6: File Names to Map
filesToMap :: [FilePath] -> Map.Map Int FilePath
filesToMap files = Map.fromList (zip [1..] files)

-- HC13T7: Use Custom Module in Main
demoSumNonEmpty :: IO ()
demoSumNonEmpty = do
    printTitle "HC13T7: Use Custom Module"
    let nums = [10, 20, 30, 40]
    putStrLn ("Sum of " ++ show nums ++ " = " ++ show (sumNonEmpty nums))

-- HC13T8: Qualified Imports for Name Conflicts
qualifiedExample :: IO ()
qualifiedExample = do
    printTitle "HC13T8: Qualified Imports"
    let listA = [3, 1, 2]
    putStrLn ("L.sort: " ++ show (L.sort listA))
    putStrLn ("M.keys: " ++ show (M.keys (M.fromList [(1, "A"), (2, "B")])))
    putStrLn "Demonstrated qualified imports."

-- HC13T9: Renaming Module Namespace
renamedModulesExample :: IO ()
renamedModulesExample = do
    printTitle "HC13T9: Renamed Module Namespace"
    let exampleList = [5, 1, 4, 2]
    putStrLn ("Sorted list (L): " ++ show (L.sort exampleList))
    let exampleMap = M.fromList [(1, "one"), (2, "two")]
    putStrLn ("Map keys (M): " ++ show (M.keys exampleMap))

-- HC13T10: Multi-Module Main Function
multiModuleMain :: IO ()
multiModuleMain = do
    printTitle "HC13T10: Multi-Module File Search and Sort"
    putStrLn "Enter a substring to search for in file names:"
    sub <- getLine
    results <- sortFilteredFiles sub
    putStrLn ("Sorted matching files: " ++ show results)

-- Menu-based main
main :: IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn "---------- Haskell Chapter 13 Practical Tasks ----------"
    putStrLn "Choose a task (1 - 10):"
    choice <- getLine
    case choice of
        "1" -> do
            printTitle "HC13T1: List Files"
            files <- listFiles
            mapM_ putStrLn files
        "2" -> do
            printTitle "HC13T2: Filter Files by Substring"
            putStrLn "Enter substring:"
            sub <- getLine
            filtered <- filterFilesBySubstring sub
            mapM_ putStrLn filtered
        "3" -> do
            printTitle "HC13T3: Sort and Return Filtered Files"
            putStrLn "Enter substring:"
            sub <- getLine
            sorted <- sortFilteredFiles sub
            mapM_ putStrLn sorted
        "4" -> do
            printTitle "HC13T4: SumNonEmpty Module"
            print (sumNonEmpty [1, 2, 3, 4])
        "5" -> do
            printTitle "HC13T5: Restricted Module Export"
            print (sumNonEmpty [5, 6, 7])
        "6" -> do
            printTitle "HC13T6: File Names to Map"
            files <- listFiles
            let fileMap = filesToMap files
            print fileMap
        "7" -> demoSumNonEmpty
        "8" -> qualifiedExample
        "9" -> renamedModulesExample
        "10" -> multiModuleMain
        _ -> putStrLn "Invalid choice. Enter 1 - 10."
