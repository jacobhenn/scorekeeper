{-# LANGUAGE MultiWayIf #-}

import Data.List
import System.IO
import Data.Either
import Control.Lens
import Control.Monad

-- a couple datatypes for fun and readability
data CustomError = CustomError { code        :: Int
                               , description :: String }

data Team = Team { score    :: Int
                 , name     :: String } deriving (Eq)

-- a couple helpful instances for those datatypes
instance Show CustomError where
    show (CustomError code description) = "E" ++ show code ++ " " ++ description

instance Show Team where
    show (Team teamScore teamName) = show teamScore ++ " " ++ teamName

instance Ord Team where
    (Team teamScore _) `compare` (Team teamScore' _) = teamScore `compare` teamScore'

-- some helper functions for updateTeams
-- take two teams with the same name and add their scores
addTeams :: Team -> Team -> Team
addTeams (Team teamScore _) (Team teamScore' teamName) = Team (teamScore + teamScore') teamName

-- a composition of putStr and getLine
prompt :: String -> IO String
prompt x = do putStr x
              hFlush stdout
              getLine

teamFromInput :: String -> [Team] -> Either Team CustomError
teamFromInput string teamList
    | length stringWords == 1 = if
        | elem (head stringWords) teamNames -> Left $ Team 1 $ head stringWords
        | otherwise                         -> Right noTeam
    | length stringWords == 2 = if
        | elem (last stringWords) teamNames -> Left $ Team (read $ head stringWords :: Int) $ last stringWords
        | otherwise                         -> Right noTeam
    | length stringWords == 0 = Right $ CustomError 2 "No arguments"
    | otherwise = Right $ CustomError 3 "Invalid input"
    where teamNames   = map name teamList
          stringWords =      words string
          noTeam      = CustomError 1 "Not a valid team name"

-- the main function that takes a new score to add and a list of Teams to update.
updateTeams :: Team -> [Team] -> [Team]
updateTeams inputTeam@(Team teamScore teamName) teamList = sort $ teamList & element teamIndex .~ newTeam
                                                           where teamNames  = map name teamList
                                                                 teamScores = map score teamList
                                                                 teamIndex  = head $ elemIndices teamName teamNames
                                                                 oldTeam    = teamList !! teamIndex
                                                                 newTeam    = addTeams oldTeam inputTeam

-- the main loop function that takes the list of teams along with it and runs updateteams on every input
loop :: [Team] -> IO ()
loop list = do
    putStrLn $ take 24 $ repeat '-'
    mapM_ print list
    line <- prompt "[score, name] "
    let convertLine = teamFromInput line list
    let isExit      = elem line ["quit", "exit"]
    when (isRight convertLine) $ do
        unless (isExit) $ do
            print $ fromRight (CustomError 0 "") convertLine
            loop list
    unless (isExit) $ do
        let newList = updateTeams (fromLeft (Team 0 "") convertLine) list
        loop newList

-- the actual main function that is called at the beginning of the program and asks the player to initialize a list of teams
main = do putStrLn "Scorekeeper sucessfully run."
          line <- prompt "[initial teams] "
          loop $ map (Team 0) $ words line
