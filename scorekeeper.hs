--------------------------------------------------------------------------------
-- Complete rewrite of my previous cli scorekeeping app using functor syntax
-- Author: Jacob Henn
--------------------------------------------------------------------------------
{-# LANGUAGE MultiWayIf #-}
import Control.Monad
import Data.Either
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Safe as S
import System.Environment
import System.IO

--------------------------------------------------------------------------------
-- team datatype for storing an integer and string
data Team = Team{score :: Int, name :: String}
              deriving (Eq, Show, Read)

instance Ord Team where
        (Team teamScore _) `compare` (Team teamScore' _)
          = teamScore `compare` teamScore'

--------------------------------------------------------------------------------
-- handy abbreviations
vec = V.fromList
(!) = (V.!)
upd x y z = V.update x $ V.cons (y, z) V.empty

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
-- remove a team of the specified name from the vector
removeTeam :: String -> V.Vector Team -> Either String (V.Vector Team)
removeTeam s ts
  = V.ifilter <$> (flip <$> const <$> ((/=) <$> index)) <*> return ts
  where index
          = toEither (s ++ " is not a team") $ V.findIndex ((s ==) . name) ts

--------------------------------------------------------------------------------
-- look up a team and modify it from an input string
addTeams :: String -> V.Vector Team -> Either String (V.Vector Team)
addTeams input teamVec
  = V.modify VA.sort <$> (upd teamVec <$> teamIndex <*> newTeam)
  where inputWords = words input
        newPoints
          = toEither (head inputWords ++ " is not a number") $
              S.readMay $ head inputWords
              :: Either String Int
        teamName = last inputWords
        teamIndex
          = toEither (last inputWords ++ " is not a team") $
              V.elemIndex teamName (V.map name teamVec)
        newTeam = addPoints <$> newPoints <*> ((teamVec !) <$> teamIndex)

--------------------------------------------------------------------------------
-- change the vector of teams based off of user input
updateTeams :: String -> V.Vector Team -> Either String (V.Vector Team)
updateTeams input teamVec
  | length inputWords == 0 = Left "no input"
  | otherwise =
    case command of
        "add" -> Right $
                   V.modify VA.sort $ V.map (Team 0) (vec args) V.++ teamVec
        "rm" -> foldl (>>=) (return teamVec) (map removeTeam args)
        otherwise -> case length inputWords of
                       1 -> addTeams ("1 " ++ command) teamVec
                       2 -> addTeams (input) teamVec
                       otherwise -> Left "too many arguments"
  where inputWords = words input
        args = tail inputWords
        command = head inputWords

--------------------------------------------------------------------------------
-- choose which IO action to do next based off of user input
ioLogic :: String -> V.Vector Team -> Either String (V.Vector Team) -> IO ()
ioLogic input teamVec newTeamVec =
    if | elem (concat $ words input) ["quit", "exit"] -> return ()
       | otherwise ->
           if | isRight newTeamVec -> loop $ fromRight undefined newTeamVec
              | otherwise -> loop teamVec

--------------------------------------------------------------------------------
-- main loop which carries the team vector with it through infinity
loop :: V.Vector Team -> IO ()
loop teamVec
  = do putStrLn $ replicate 24 '-'
       V.mapM_ putStrLn $ V.map (\ (Team s n) -> show s ++ " " ++ n) teamVec
       input <- prompt "[scorekeeper] "
       ioLogic input teamVec $ updateTeams input teamVec

--------------------------------------------------------------------------------
-- main function that's called first but can't be a loop because of its set type
main :: IO ()
main
  = do putStrLn "enter \"help\" to see a list of commands"
       args <- vec <$> getArgs
       loop $ V.map (Team 0) args
