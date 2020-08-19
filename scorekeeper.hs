{-# LANGUAGE MultiWayIf #-}
import Control.Lens
import Control.Monad

import Data.Either
import Data.List
import Data.Maybe
import Safe
import System.Directory
import System.Environment
import System.IO

saveDir :: String
-- EDIT THE VARIABLE BELOW TO CHANGE THE DIRECTORY YOU WANT TO STORE YOUR SAVED SCORES
saveDir = "/home/jacob/.config/scorekeeper/saves/"

-- EDIT THE VARIABLE ABOVE TO CHANGE THE DIRECTORY YOU WANT TO STORE YOUR SAVED SCORES

data CustomError = CustomError{code :: Int, description :: String}

data Team = Team{score :: Int, name :: String}
              deriving (Eq)

-- a couple helpful instances for those datatypes
instance Show CustomError where
        show (CustomError code description)
          = "E" ++ show code ++ " " ++ description

instance Show Team where
        show (Team teamScore teamName) = show teamScore ++ " " ++ teamName

instance Ord Team where
        (Team teamScore _) `compare` (Team teamScore' _)
          = teamScore `compare` teamScore'

nameEq :: Team -> Team -> Bool
nameEq team team'
  | name team == name team' = True
  | otherwise = False

-- some helper functions for updateTeams
-- take two teams with the same name and add their scores
addTeams :: Team -> Team -> Team
addTeams (Team teamScore _) (Team teamScore' teamName)
  = Team (teamScore + teamScore') teamName

-- an IO-safe composition of putStr and getLine
prompt :: String -> IO String
prompt x
  = do putStr x
       hFlush stdout
       getLine

-- takes a string and parses it into a team. fails gracefully if the input is invalid.
teamFromInput :: String -> [Team] -> Either Team CustomError
teamFromInput string teamList
  = if | length stringWords == 0 -> Right $ CustomError 2 "No arguments"
       | elem (head stringWords)
           ["add", "rm", "help", "quit", "exit", "save", "load", "ls", "acc"]
         -> Right $ CustomError 0 ""
       | length stringWords == 1 ->
         if | elem (head stringWords) teamNames ->
              Left $ Team 1 $ head stringWords
            | otherwise -> Right $ CustomError 3 "Invalid input"
       | length stringWords == 2 ->
         if | isJust (readMay $ head stringWords :: Maybe Int) &&
                elem (last stringWords) teamNames
              -> Left $ Team (read $ head stringWords :: Int) $ last stringWords
            | otherwise -> Right $ CustomError 3 "Invalid team name or number"
       | otherwise -> Right $ CustomError 6 "Too many arguments"
  where teamNames = map name teamList
        stringWords = words string
        noTeam = CustomError 1 "Not a valid team name"

-- the main function that takes a new score to add and a list of Teams to update.
updateTeams :: Team -> [Team] -> [Team]
updateTeams inputTeam@(Team teamScore teamName) teamList
  = sort $ teamList & element teamIndex .~ newTeam
  where teamNames = map name teamList
        teamScores = map score teamList
        teamIndex = head $ elemIndices teamName teamNames
        oldTeam = teamList !! teamIndex
        newTeam = addTeams oldTeam inputTeam

-- appends one or more blank teams to a team list
appendTeams :: [String] -> [Team] -> [Team]
appendTeams stringWordsTail teamList
  = sort $ teamList ++ map (Team 0) stringWordsTail

removeTeams :: [String] -> [Team] -> [Team]
removeTeams stringWordsTail teamList
  = deleteFirstsBy nameEq teamList $ map (Team 0) stringWordsTail

-- turn a string read from a file encoded with encodeSave and turn it back into a list of teams
parseSave :: String -> [Team]
parseSave string
  = zipWith (Team) (map read $ map head stringWords :: [Int]) $
      map last stringWords
  where stringWords = map words $ lines string

-- turn the team list into something that can be written to a file
encodeSave :: [Team] -> String
encodeSave teamList = unlines $ map show teamList

-- take two lists of teams with the same names and add their scores together
accumulateSave :: [Team] -> [Team] -> [Team]
accumulateSave accList newList = zipWith (Team) newScores teamNames
  where teamNames = map name newList
        addScores = map score newList
        accScores = map score accList
        newScores = zipWith (+) addScores accScores

-- a convenient helper function in which to stick some of the IO logic to make it pure and easier to deal with
ioTree :: String -> [Team] -> IO ()
ioTree string teamList
  = if | isLeft stringTeam -> loop $ updateTeams (stringTeamFromLeft) teamList
       | otherwise ->
         if | code stringTeamFromRight == 0 ->
              if | head stringWords == "add" ->
                   if | length stringWords > 1 ->
                        if | or [elem x teamNames | x <- tail stringWords] ->
                             do print $
                                  CustomError 4
                                    "One of the names provided is already a team"
                                loop teamList
                           | otherwise ->
                             loop $ appendTeams (tail stringWords) teamList
                           | otherwise ->
                             do print $ CustomError 2 "No arguments"
                                loop teamList
                      | otherwise ->
                        do print $ CustomError 2 "No arguments"
                           loop teamList
                 | head stringWords == "rm" ->
                   if | length stringWords > 1 ->
                        if | not $
                               and [elem x teamNames | x <- tail stringWords]
                             ->
                             do print $
                                  CustomError 5
                                    "One of the names provided is not a team"
                                loop teamList
                           | otherwise ->
                             loop $ removeTeams (tail stringWords) teamList
                      | otherwise ->
                        do print $ CustomError 2 "No arguments"
                           loop teamList
                 | head stringWords == "save" ->
                   if | length stringWords == 2 ->
                        if | head stringWords == "save" ->
                             do writeFile (saveDir ++ last stringWords) $
                                  encodeSave teamList
                                loop teamList
                      | otherwise ->
                        if | length stringWords < 2 ->
                             do print $ CustomError 2 "No arguments"
                                loop teamList
                           | otherwise ->
                             do print $ CustomError 6 "Too many arguments"
                                loop teamList
                 | head stringWords == "load" ->
                   if | length stringWords == 2 ->
                        do saves <- listDirectory saveDir
                           if (elem (last stringWords) saves) then
                             do rawSave <- readFile $
                                             saveDir ++ last stringWords
                                loop $ parseSave rawSave
                             else
                             do print $
                                  CustomError 7 $
                                    "Save file \"" ++
                                      last stringWords ++ "\" does not exist"
                                loop teamList
                      | otherwise ->
                        if | length stringWords < 2 ->
                             do print $ CustomError 2 "No arguments"
                                loop teamList
                           | otherwise ->
                             do print $ CustomError 6 "Too many arguments"
                                loop teamList
                 | head stringWords == "ls" ->
                   do saves <- listDirectory saveDir
                      putStr $ unlines saves
                      loop teamList
                 | head stringWords == "help" ->
                   do putStrLn $ replicate 24 '-'
                      putStr $
                        unlines
                          ["change a team's score:                                                        ",
                           "  enter just a team name                - add 1 to that team                  ",
                           "  enter an integer and then a team name - add that integer to that team       ",
                           "edit the list of teams:                                                       ",
                           "  enter \"add\" then one or more team names - add those teams to the list     ",
                           "  enter \"del\" then one or more team names - remove those teams from the list",
                           "show this message: enter \"help\"                                             "]
                      loop teamList
                 | elem (head stringWords) ["quit", "exit"] -> return ()
                 | otherwise ->
                   do print $ CustomError 3 "Invalid input"
                      loop teamList
            | otherwise ->
              do print $ stringTeamFromRight
                 loop teamList
  where stringTeam = teamFromInput string teamList
        stringWords = words string
        stringTeamFromRight = fromRight (CustomError 0 "") stringTeam
        stringTeamFromLeft = fromLeft (Team 0 "") stringTeam
        teamNames = map name teamList

-- the main loop function that takes the list of teams along with it and runs updateteams on every input
loop :: [Team] -> IO ()
loop list
  = do putStrLn $ replicate 24 '-'
       mapM_ print list
       line <- prompt "[scorekeeper] "
       ioTree line list

-- the actual main function that is called at the beginning of the program and asks the player to initialize a list of teams
main
  = do putStrLn "Enter \"help\" to see a list of commands."
       args <- getArgs
       loop $ map (Team 0) args
