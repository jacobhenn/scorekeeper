--------------------------------------------------------------------------------
-- Complete rewrite of my previous cli scorekeeping app using functor syntax
-- Author: Jacob Henn
--------------------------------------------------------------------------------
{-# LANGUAGE MultiWayIf #-}
import Prelude hiding (replicate, length, filter)
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Sequence
import qualified Safe as S
import System.Environment
import System.IO
import qualified Data.List as L

--------------------------------------------------------------------------------
-- team datatype for storing an integer and string
data Team = Team{score :: Int, name :: String}
              deriving (Eq, Show, Read)

instance Ord Team where
        (Team teamScore _) `compare` (Team teamScore' _)
          = teamScore `compare` teamScore'

--------------------------------------------------------------------------------
-- string printed by the help command
helpString :: String
helpString
  = unlines $
      ["change a team's score:",
       "  enter just a team name                - add 1 to that team",
       "  enter an integer and then a team name - add that integer to that team",
       "edit the list of teams:",
       "  enter \"add\" then one or more team names - add those teams to the list",
       "  enter \"rm\" then one or more team names  - remove those teams from the list",
       -- "manipulate save files:",
       -- "  enter \"load\" then a save file name - discard the current scores and load teams from the file",
       -- "  enter \"save\" then a name           - save the current scores to a file of that name, and overwrite if the file already exists",
       -- "  enter \"ls\"                         - list all save files",
       "show this message: enter \"help\""]

--------------------------------------------------------------------------------
-- in this case, I want Eithers instead of Maybes
toEither :: a -> Maybe b -> Either a b
toEither x y = fromMaybe (Left x) $ Right <$> y

--------------------------------------------------------------------------------
-- reliable prompt function
prompt :: String -> IO String
prompt x
  = do putStr x
       hFlush stdout
       getLine

--------------------------------------------------------------------------------
-- add points to a team
addPoints :: Int -> Team -> Team
addPoints x (Team a b) = Team (a + x) b

--------------------------------------------------------------------------------
-- remove a team of the specified name from the sequence
removeTeam :: String -> Seq Team -> Seq Team
removeTeam s ts = filter (\ x -> name x /= s) ts

--------------------------------------------------------------------------------
-- look up a team and modify it from an input string
addTeams :: String -> Seq Team -> Either String (Seq Team)
addTeams input teamSeq
  = sort <$> (update <$> teamIndex <*> newTeam <*> return teamSeq)
  where inputWords = words input
        newPoints
          = toEither (head inputWords ++ " is not a number") $
              S.readMay $ head inputWords
              :: Either String Int
        teamName = last inputWords
        teamIndex
          = toEither (teamName ++ " is not a team") $
              elemIndexL teamName $ name <$> teamSeq
        newTeam = addPoints <$> newPoints <*> (index teamSeq <$> teamIndex)

--------------------------------------------------------------------------------
-- change the sequence of teams based off of user input
updateTeams :: String -> Seq Team -> Either String (Seq Team)
updateTeams input teamSeq
  | L.null inputWords = Left "no input"
  | otherwise =
    case command of
        "add" -> Right $
                   sort $ teamSeq >< ((Team 0) <$> fromList args)
        "rm" -> Right $ L.foldl1' (.) (map removeTeam args) $ teamSeq
        otherwise -> case L.length inputWords of
                         1 -> addTeams ("1 " ++ command) teamSeq
                         2 -> addTeams (input) teamSeq
                         otherwise -> Left "too many arguments"
  where inputWords = words input
        args = tail inputWords
        command = head inputWords

--------------------------------------------------------------------------------
-- choose which IO action to do next based off of user input
ioLogic :: String -> Seq Team -> Either String (Seq Team) -> IO ()
ioLogic input teamSeq newTeamSeq
  = if | elem input ["quit", "exit"] -> return ()
       | input == "help" ->
         do putStr helpString
            loop teamSeq
       | otherwise ->
         if | isRight newTeamSeq -> loop $ fromRight undefined newTeamSeq
            | otherwise ->
              do putStrLn $ "Error: " ++ fromLeft undefined newTeamSeq
                 loop teamSeq
  where inputWords = words input
        command = head inputWords
        args = tail inputWords

--------------------------------------------------------------------------------
-- main loop which carries the team sequence with it through infinity
loop :: Seq Team -> IO ()
loop teamSeq
  = do putStrLn $ L.replicate 24 '-'
       putStrLn $ foldl1 (\x y -> x ++ "\n" ++ y) $ (\ (Team s n) -> show s ++ " " ++ n) <$> teamSeq
       input <- prompt "[scorekeeper] "
       ioLogic input teamSeq $ updateTeams input teamSeq

--------------------------------------------------------------------------------
-- main function that's called first but can't be a loop because of its set type
main :: IO ()
main
  = do putStrLn "enter \"help\" to see a list of commands"
       args <- fromList <$> getArgs
       loop $ (Team 0) <$> args
