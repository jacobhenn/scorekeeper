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
    "╭┴──────────────────────────────────────────────────────────────────╮\n\
    \│ scorekeeper v5.0.0 (https://github.com/jacobhenn/scorekeeper)     │\n\
    \├───────────────────┬───────────────────────────────────────────────┤\n\
    \│ [player]          │ add 1 point to [player]                       │\n\
    \│ [n] [player]      │ add [n] (integer) points to [player]          │\n\
    \│ :add [player] ... │ add [player](s) to the list of players        │\n\
    \│ :rm [player] ...  │ remove [player](s) from the list of players   │\n\
    \│ :mul [n]          │ multiply each player's score by [n] (integer) │\n\
    \│ :q, :quit         │ exit scorekeeper                              │\n\
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
printError :: String -> InputT IO ()
printError string = outputStrLn $ " │ ↑ error: " ++ string

--------------------------------------------------------------------------------
toEither :: a -> Maybe b -> Either a b
toEither x = maybe (Left x) Right

--------------------------------------------------------------------------------
readEither :: Read a => String -> String -> Either String a
readEither m x = toEither ("couldn't parse '" ++ x ++ "': " ++ m) $ readMay x

--------------------------------------------------------------------------------
ask :: String -> IO String
ask prompt = putStr prompt >> hFlush stdout >> getLine

--------------------------------------------------------------------------------
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
mulPlayers :: [String] -> [Player] -> Either String [Player]
mulPlayers args players = case args of
    [] -> Left "no arguments"
    [arg] -> do
        c <- readEither "not a number" arg
        Right $ map (\(Player s n)-> Player (c*s) n) players
    (_:a:_) -> Left $ "unexpected argument '"++a++"'"

--------------------------------------------------------------------------------
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
addPoints :: Int -> String -> [Player] -> Either String [Player]
addPoints pts search players = case is of
    [] -> Left $ '\'' : search ++ "' doesn't match any players"
    [(i,Player s n)] -> Right $ take i players
                             ++ [Player (s+pts) n]
                             ++ drop (i+1) players
    _ -> Left $ '\'' : search ++ "' matches more than one player"
    where is = filter (isPrefixOf search . name . snd) $ zip [0..] players

--------------------------------------------------------------------------------
parseAddPoints :: String -> [Player] -> Either String [Player]
parseAddPoints input players = case (length inputWords) of
    1 -> addPoints 1 search players
    _ -> case points of
        Right p -> addPoints p search players
        Left e  -> Left e
    where inputWords = words input
          points = readEither "not a number" $ head inputWords
          search = last inputWords

--------------------------------------------------------------------------------
parse :: String -> [Player] -> Either String [Player]
parse input players
    | null (words input) = Left "no input"
    | head input == ':'  = parseCommand (tail input) players
    | otherwise          = parseAddPoints input players

--------------------------------------------------------------------------------
-- takes the player list as well as user input and decides how to process it
process :: [Player] -> String -> InputT IO ()
process players input
    | elem input [":q", ":quit"] = return ()
    | elem input [":h", ":help"] = printHelp >> loop players
    | otherwise = case (parse input players) of
        Right newPlayers -> loop newPlayers
        Left msg -> printError msg >> loop players

--------------------------------------------------------------------------------
-- main loop; carries the player list with it through infinity
loop :: [Player] -> InputT IO ()
loop players = do
    lift $ if null players then return () else printPlayers players
    lineMay <- getInputLine " │ "
    case lineMay of
        Nothing -> printError "unexpected input"
        Just line -> process players line

--------------------------------------------------------------------------------
-- initialize the players from command-line arguments
main :: IO ()
main = do
    putStrLn " ╭─────────────────────────────────────"
    putStrLn " │ enter ':help' for a list of commands"
    runInputT defaultSettings $ loop [] -- =<< map (Player 0) <$> getArgs
