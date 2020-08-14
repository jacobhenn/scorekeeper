{-# LANGUAGE MultiWayIf #-}

import Safe
import Data.List
import System.IO
import Data.Maybe
import Data.Either
import Control.Lens
import Control.Monad
import System.Environment

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

nameEq :: Team -> Team -> Bool
nameEq team team'
    | name team == name team' = True
    | otherwise               = False

-- some helper functions for updateTeams
-- take two teams with the same name and add their scores
addTeams :: Team -> Team -> Team
addTeams (Team teamScore _) (Team teamScore' teamName) = Team (teamScore + teamScore') teamName

-- an IO-safe composition of putStr and getLine
prompt :: String -> IO String
prompt x = do
    putStr x
    hFlush stdout
    getLine

-- takes a string and parses it into a team. fails gracefully if the input is invalid.
teamFromInput :: String -> [Team] -> Either Team CustomError
teamFromInput string teamList
    | length stringWords == 0 = Right $ CustomError 2 "No arguments"
    | elem (head stringWords) ["add", "del", "help", "quit", "exit"] = Right $ CustomError 0 ""
    | length stringWords == 1 = if
        | elem (head stringWords) teamNames -> Left $ Team 1 $ head stringWords
        | otherwise                         -> Right $ CustomError 3 "Invalid input"
    | length stringWords == 2 = if
        | isJust (readMay $ head stringWords :: Maybe Int) && elem (last stringWords) teamNames -> Left  $ Team (read $ head stringWords :: Int) $ last stringWords
        | otherwise                                                                             -> Right $ CustomError 3 "Invalid team name or number"
    | otherwise = Right $ CustomError 6 "Too many arguments"

    where teamNames   = map name teamList
          stringWords = words string
          noTeam      = CustomError 1 "Not a valid team name"

-- the main function that takes a new score to add and a list of Teams to update.
updateTeams :: Team -> [Team] -> [Team]
updateTeams inputTeam@(Team teamScore teamName) teamList =
    sort $ teamList & element teamIndex .~ newTeam

    where teamNames  = map name teamList
          teamScores = map score teamList
          teamIndex  = head $ elemIndices teamName teamNames
          oldTeam    = teamList !! teamIndex
          newTeam    = addTeams oldTeam inputTeam

-- appends one or more blank teams to a team list
appendTeams :: [String] -> [Team] -> [Team]
appendTeams stringWordsTail teamList =
    sort $ teamList ++ map (Team 0) stringWordsTail

removeTeams :: [String] -> [Team] -> [Team]
removeTeams stringWordsTail teamList =
    deleteFirstsBy nameEq teamList $ map (Team 0) stringWordsTail

-- a convenient helper function in which to stick some of the IO logic to make it pure and easier to deal with
ioTree :: String -> [Team] -> IO ()
ioTree string teamList
    | isLeft stringTeam = loop $ updateTeams (stringTeamFromLeft) teamList
    | otherwise = if
        | code stringTeamFromRight == 0 -> if
            | head stringWords == "add" -> if
                | length stringWords > 1 -> if
                    | or [elem x teamNames | x <- tail stringWords] -> do
                        print $ CustomError 4 "One of the names provided is already a team"
                        loop teamList
                    | otherwise -> loop $ appendTeams (tail stringWords) teamList
                | otherwise -> do
                    print $ CustomError 2 "No arguments"
                    loop teamList
            | head stringWords == "del" -> if
                | length stringWords > 1 -> if
                    | not $ and [elem x teamNames | x <- tail stringWords] -> do
                        print $ CustomError 5 "One of the names provided is not a team"
                        loop teamList
                    | otherwise -> loop $ removeTeams (tail stringWords) teamList
                | otherwise -> do
                    print $ CustomError 2 "No arguments"
                    loop teamList
            | head stringWords == "help" -> do
                putStrLn $ replicate 24 '-'
                putStr $ unlines ["change a team's score:                                                        "
                                 ,"  enter just a team name                - add 1 to that team                  "
                                 ,"  enter an integer and then a team name - add that integer to that team       "
                                 ,"edit the list of teams:                                                       "
                                 ,"  enter \"add\" then one or more team names - add those teams to the list     "
                                 ,"  enter \"del\" then one or more team names - remove those teams from the list"
                                 ,"show this message: enter \"help\"                                             "]
                loop teamList
            | elem (head stringWords) ["quit", "exit"] -> return ()
            | otherwise -> do
                print $ CustomError 3 "Invalid input"
                loop teamList
        | otherwise -> do
            print $ stringTeamFromRight
            loop teamList

    where stringTeam          = teamFromInput string teamList
          stringWords         = words string
          stringTeamFromRight = fromRight (CustomError 0 "") stringTeam
          stringTeamFromLeft  = fromLeft  (Team        0 "") stringTeam
          teamNames           = map name teamList

-- the main loop function that takes the list of teams along with it and runs updateteams on every input
loop :: [Team] -> IO ()
loop list = do
    putStrLn $ replicate 24 '-'
    mapM_ print list
    line <- prompt "[scorekeeper] "
    ioTree line list

-- the actual main function that is called at the beginning of the program and asks the player to initialize a list of teams
main = do
    putStrLn "Enter \"help\" to see a list of commands."
    args <- getArgs
    loop $ map (Team 0) args
