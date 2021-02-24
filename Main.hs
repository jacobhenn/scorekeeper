--------------------------------------------------------------------------------
-- Slick cli scorekeeping app making use of the box drawing block
-- Author: Jacob Henn
--------------------------------------------------------------------------------
module Main where

{-# LANGUAGE MultiWayIf #-}
import System.Console.Haskeline
import Control.Monad.Trans (lift)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Maybe (maybe)
import System.IO (hFlush, stdout)
import Data.List (sort, isPrefixOf)
import Safe (readMay, headMay, maximumDef)

--------------------------------------------------------------------------------
data Player = Player{score :: Int, name :: String} deriving (Eq, Show, Read)

-- order players by their scores
instance Ord Player where
    (Player s _) `compare` (Player s' _) = s `compare` s'

--------------------------------------------------------------------------------
printHelp :: InputT IO ()
printHelp = outputStrLn
    "╭┴──────────────────────────────────────────────────────────────╮\n\
    \│ scorekeeper v5.0.3 (https://github.com/jacobhenn/scorekeeper) │\n\
    \├───────────────────────────────────────────────────────────────┤\n\
    \│ prefix searches match players whose names start with the      │\n\
    \│ given string: b = bob, ill != bill                            │\n\
    \├─────────────────┬─────────────────────────────────────────────┤\n\
    \│ [prefix]        │ add 1 point to the player matching [prefix] │\n\
    \│ [n] [prefix]    │ add [n] points to [prefix]                  │\n\
    \│ :add [player].. │ add [player](s) to the list of players      │\n\
    \│ :rm [prefix]..  │ remove [prefix](s) from the list of players │\n\
    \│ :mul [n]        │ multiply each player's score by [n]         │\n\
    \│ :q, :quit       │ exit scorekeeper                            │\n\
    \│ :h, :help       │ show this message                           │\n\
    \╰┬────────────────┴─────────────────────────────────────────────╯"

--------------------------------------------------------------------------------
-- print a formatted unicode table of the players' names and scores
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
printError :: String -> InputT IO ()
printError string = outputStrLn $ " │ ↑ error: " ++ string

--------------------------------------------------------------------------------
toEither :: a -> Maybe b -> Either a b
toEither x = maybe (Left x) Right

--------------------------------------------------------------------------------
-- like Safe.readMay, but takes an error string and returns an Either
readEither :: Read a => String -> String -> Either String a
readEither m x = toEither ("couldn't parse '" ++ x ++ "': " ++ m) $ readMay x

--------------------------------------------------------------------------------
-- parse :add [name]..
addPlayers :: [String] -> [Player] -> Either String [Player]
addPlayers args players = do
    if null args then Left "no arguments" else Right ()

    sequence $ map (\arg->
        if elem arg $ map name players
            then Left $ arg++" is already a player"
            else Right ()
        ) args

    Right $ sort $ players ++ map (Player 0) args

--------------------------------------------------------------------------------
-- parse :rm [prefix]..
rmPlayers :: [String] -> [Player] -> Either String [Player]
rmPlayers args players = do
    if null args then Left "no arguments" else Right ()

    sequence $ map (\arg->
        case length $ filter (isPrefixOf arg . name) players of
            0 -> Left $ '\'':arg++"' doesn't match any players"
            1 -> Right ()
            _ -> Left $ '\'':arg++"' matches more than one player"
        ) args

    Right $ filter (\(Player _ n)-> any (\a-> not $ a `isPrefixOf` n) args) players

--------------------------------------------------------------------------------
-- parse :mul [n]
mulPlayers :: [String] -> [Player] -> Either String [Player]
mulPlayers args players = case args of
    [] -> Left "no arguments"
    [arg] -> do
        c <- readEither "not a number" arg
        Right $ map (\(Player s n)-> Player (c*s) n) players
    (_:a:_) -> Left $ "unexpected argument '"++a++"'"

--------------------------------------------------------------------------------
-- parse user input of the form :[command] [arg]..
parseCommand :: String -> [Player] -> Either String [Player]
parseCommand input players = do
    command <- toEither "no command given" $ headMay inputWords

    case command of
        "add" -> addPlayers args players
        "rm"  -> rmPlayers args players
        "mul" -> mulPlayers args players
        _ -> Left $ '\'':command++"' is not a command"

    where inputWords = words input
          args       = tail inputWords

--------------------------------------------------------------------------------
-- find a player matching a prefix and add points to them
addPoints :: Int -> String -> [Player] -> Either String [Player]
addPoints pts search players = case is of
    [] -> Left $ '\'':search++"' doesn't match any players"
    [(i,Player s n)] -> Right $ sort $
        take i players ++ [Player (s+pts) n] ++ drop (i+1) players
    _ -> Left $ '\'':search++"' matches more than one player"
    where is = filter (isPrefixOf search . name . snd) $ zip [0..] players

--------------------------------------------------------------------------------
-- parse user input of the form [n] [prefix] or [prefix]
parseAddPoints :: String -> [Player] -> Either String [Player]
parseAddPoints input players = case length inputWords of
    1 -> addPoints 1 search players
    2 -> do p <- points
            addPoints p search players
    _ -> Left $ "unexpected argument '"++inputWords!!2++"'"
    where inputWords = words input
          points = readEither "not a number" $ head inputWords
          search = last inputWords

--------------------------------------------------------------------------------
-- construct a player list transformation from user input
parse :: String -> [Player] -> Either String [Player]
parse input players
    | null (words input) = Left "no input"
    | head input == ':'  = parseCommand (tail input) players
    | otherwise          = parseAddPoints input players

--------------------------------------------------------------------------------
-- takes the player list as well as user input and decides how to process it
process :: [Player] -> String -> InputT IO ()
process players input
    | elem cmd [":q", ":quit"] = return ()
    | elem cmd [":h", ":help"] = printHelp >> loop players
    | otherwise = case parse input players of
        Right newPlayers -> loop newPlayers
        Left msg -> printError msg >> loop players
    where cmd = head $ words input

--------------------------------------------------------------------------------
-- main loop; carries the player list with it through infinity
loop :: [Player] -> InputT IO ()
loop players = do
    lift $ if null players then return () else printPlayers players
    lineMay <- getInputLine " │ "
    case lineMay of
        Nothing -> outputStrLn "error: input was given in an unexpected way"
        Just line -> process players line

--------------------------------------------------------------------------------
-- initialize the players from command-line arguments
main :: IO ()
main = do
    putStrLn " ╭─────────────────────────────────────"
    putStrLn " │ enter ':help' for a list of commands"
    args <- getArgs
    runInputT defaultSettings $ loop $ map (Player 0) args
