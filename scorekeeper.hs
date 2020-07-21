import Data.List
import System.IO
import Control.Lens
import Control.Monad

-- a couple datatypes for readability
data CustomError = CustomError { code        :: Int
                               , description :: String }

data Team = Team { score    :: Int
                 , name     :: String
                 } deriving (Eq, Show)

-- a couple helpful instances for those datatypes
instance Show CustomError where
    show CustomError code description = "E" ++ show code ++ " " ++ description

instance Show Team where
    show Team score name = (show score) ++ " " ++ name

instance Ord Team where
    (Team score _) `compare` (Team score' _) = score `compare` score'

-- a couple helpful methods on those datatypes
teamFromInput :: [String] -> Team
teamFromInput [score, name] = Team (read score :: Int) name

-- some helper functions for updateTeams
-- take two teams with the same name and add their scores
addTeams :: Team -> Team -> Team
addTeams (Team score _) (Team score' name) = Team (score + score') name

-- a composition of putStr and getLine
prompt :: String -> IO String
prompt x = do putStr x
              hFlush stdout
              getLine

-- a helper function that takes a previous value, a prompt, a function that turns an input string into a new list of values, a function to take a list of these values and convert it to a string, and an exit condition, and returns an IO monad that can be used to mightily compact IO loops.
interactive :: [a] -> String -> (String -> [a] -> [a]) -> ([a] -> String) -> (String -> Bool) -> IO ()
interactive list promptString function formatter exitCondition = do
    line <- prompt promptString
    unless (exitCondition line) $ do
        let newList = function line list
        interactive newList promptString function formatter exitCondition

-- puts a list of teams in order from greatest to least scores
orderTeams :: [Team] -> [Team]


-- the main function that takes a new score to add and a list of Teams to update.
updateTeams :: Team -> [Team] -> [Team]
updateTeams inputTeam@(Team teamScore teamName) teamList = teamList & (!! teamIndex) .~ newTeam
                                                            where teamNames  =                   map (name) teamList
                                                                  teamScores =                  map (score) teamList
                                                                  teamIndex  = head $ elemIndices teamName teamNames
                                                                  oldTeam    =                 teamList !! teamIndex
                                                                  newTeam    =            addTeams oldTeam inputTeam

-- the actual main function that is called at the beginning of the program and asks the player to initialize a list of teams
main = do putStrLn "Scorekeeper sucessfully run."
          line <- prompt "[initial teams] "
          let initialTeams = map (Team 0) $ words line
          interactive initialTeams "[score, name] " (orderTeams . updateTeams)
