--------------------------------------------------------------------------------
-- Slick cli scorekeeping app making use of the box drawing block
-- Author: Jacob Henn
--------------------------------------------------------------------------------
module Main where

{-# LANGUAGE MultiWayIf #-}
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Monad (liftM2)
import Data.Either (fromRight)
import Text.Printf (printf)
import Data.Maybe (maybe)
import System.IO (hFlush, stdout, writeFile, readFile)
import Data.List (sort, isPrefixOf)
import Safe (readMay, headMay, atMay, foldl1May, maximumDef)

--------------------------------------------------------------------------------
data Player = Player{score :: Int, name :: String} deriving (Eq, Show, Read)

-- order players by their scores
instance Ord Player where
    (Player s _) `compare` (Player s' _) = s `compare` s'

--------------------------------------------------------------------------------
printHelp :: IO ()
printHelp = putStrLn "╭┴──────────────────────────────────────────────────────────────────╮\n\
                     \│ scorekeeper v3.4.0 (https://github.com/jacobhenn/scorekeeper)     │\n\
                     \├───────────────────┬───────────────────────────────────────────────┤\n\
                     \│ [player]          │ add 1 point to [player]                       │\n\
                     \│ [n] [player]      │ add [n] (integer) points to [player]          │\n\
                     \│ :add [player] ... │ add [player](s) to the list of players        │\n\
                     \│ :rm [player] ...  │ remove [player](s) from the list of players   │\n\
                     \│ :mul [n]          │ multiply each player's score by [n] (integer) │\n\
                     \│ :q, :quit, :exit  │ exit scorekeeper                              │\n\
                     \│ :h, :help         │ show this message                             │\n\
                     \╰┬──────────────────┴───────────────────────────────────────────────╯"

--------------------------------------------------------------------------------
printPlayers :: [Player] -> IO ()
printPlayers players = do
    putStrLn $ "╭┴" ++ replicate maxNameLength '─'
                   ++ "─┬─"
                   ++ replicate maxScoreLength '─'
                   ++ "─╮"

    mapM_ (\(Player s n) -> printf "│ %-*s │ %*d │\n" maxNameLength n maxScoreLength s)
        players

    putStrLn $ "╰┬" ++ replicate maxNameLength '─'
                   ++ "─┴─"
                   ++ replicate maxScoreLength '─'
                   ++ "─╯"

    where maxNameLength  = maximumDef 0 $ map (length . name)         players
          maxScoreLength = maximumDef 0 $ map (length . show . score) players

--------------------------------------------------------------------------------
printError :: String -> IO ()
printError string = putStrLn $ " │ ↑ Error: " ++ string

--------------------------------------------------------------------------------
toEither :: a -> Maybe b -> Either a b
toEither x = maybe (Left x) Right

--------------------------------------------------------------------------------
ask :: String -> IO String
ask prompt = do
    putStr prompt
    hFlush stdout
    getLine

--------------------------------------------------------------------------------
-- append the current scores to a log file; errors gracefully
-- writeLogFile :: FilePath -> [Player] -> Either String (IO ())
-- writeLogFile path players = (\exists -> if
--     | exists -> writeFile path
--         =<< show . (players:) -- ::
--         =<< (toEither (path++" is not a valid log file") . readMay) -- :: Either String (IO [Player])
--         <$> readFile path -- :: IO String
--     | _ -> Left $ path ++ " does not exist"
--     ) <$> doesFileExist path

--------------------------------------------------------------------------------
addPlayer :: String -> [Player] -> [Player]
addPlayer playerName = sort . (Player 0 playerName :)

--------------------------------------------------------------------------------
rmPlayer :: String -> [Player] -> [Player]
rmPlayer playerName = sort . filter ((playerName/=) . name)

--------------------------------------------------------------------------------
-- execute an input of the form '[int] [team name]' on the team list
addPoints :: Int -> String -> [Player] -> [Player]
addPoints points playerName players =
    sort $ map (\(Player s n) ->
        if | playerName `isPrefixOf` n -> Player (s + points) n
           | otherwise -> Player s n
    ) players

--------------------------------------------------------------------------------
mulPoints :: Int -> [Player] -> [Player]
mulPoints c = map (\(Player s n) -> Player (c*s) n)

--------------------------------------------------------------------------------
-- parse an input of the form '[int] [team name]' into a transformation
parseAddPoints :: String -> Either String ([Player] -> [Player])
parseAddPoints input = case (length inputWords) of
    1 -> Right $ addPoints 1 player
    _ -> liftM2 addPoints points $ return player
    where inputWords = words input
          points = toEither (head inputWords ++ " is not a number")
                            (readMay $ head inputWords :: Maybe Int)
          player = last inputWords

--------------------------------------------------------------------------------
-- parse a ':' command into a team list transformation; errors gracefully
parseCommand :: String -> Either String ([Player] -> [Player])
parseCommand input = (\x -> case x of
        "add" -> toEither "no arguments" $ foldl1May (.) $ map addPlayer args
        "rm"  -> toEither "no arguments" $ foldl1May (.) $ map rmPlayer  args
        "mul" -> mulPoints <$>
            ((toEither (fromRight undefined (arg 0) ++" is not a number") . readMay)
            =<< arg 0)
        _ -> Left $ head inputWords ++" is not a valid command"
    ) =<< command
    where inputWords = words input
          command = toEither "empty command" $ headMay inputWords
          args    = tail inputWords
          arg i   = toEither "insufficient arguments" $ atMay args i

--------------------------------------------------------------------------------
-- turns valid input into a transformation of the team list; errors gracefully
parse :: String -> Either String ([Player] -> [Player])
parse input = if
    | null inputWords   -> Left "no input"
    | head input == ':' -> parseCommand $ tail input
    | otherwise         -> parseAddPoints input
    where inputWords = words input

--------------------------------------------------------------------------------
-- takes the team list as well as user input and decides how to process it
process :: [Player] -> String -> IO ()
process players input
    | elem input [":q", ":quit", ":exit"] = return ()
    | elem input [":h", ":help"] = do
        printHelp
        loop players
    | otherwise = either
        (\y -> (\x -> loop players) =<< printError y)
        (loop . ($players))
        $ parse input

--------------------------------------------------------------------------------
-- main loop; carries the team list with it through infinity
loop :: [Player] -> IO ()
loop players = do
    printPlayers players
    process players =<< ask " │ "

--------------------------------------------------------------------------------
-- initialize the teams from command-line arguments
main :: IO ()
main = do
    putStrLn " ╭─────────────────────────────────────"
    putStrLn " │ enter ':help' for a list of commands"
    loop =<< map (Player 0) <$> getArgs
