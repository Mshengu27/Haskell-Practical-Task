import Data.List (sortBy)
import Data.Ord (comparing)

-- HC1T4 - Task 4: Composing a Function to Process Player Data

-- Extract player names
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

-- Sort players by score (descending)
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

-- Take the top three players
topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

-- Compose functions to get top three player names
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Main to test HC1T4
main :: IO ()
main = do
    putStrLn "HC1T4 - Get Top Three Players"
    let players = [("Alice", 50), ("Bob", 80), ("Charlie", 70), ("Diana", 90), ("Eve", 60)]
    putStrLn $ "All players: " ++ show players
    putStrLn $ "Top three players: " ++ show (getTopThreePlayers players)
