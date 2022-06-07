module Project where

import Text.Read (readMaybe)

type Task = String

tasksToString :: [Task] -> String
tasksToString [] = ""
tasksToString (task:tasks) = task ++ "\n" ++ tasksToString tasks

printAllTasks :: [Task] -> IO ()
printAllTasks [] = putStrLn ""
printAllTasks (task:tasks) = do
    putStrLn task
    printAllTasks tasks

removeTask :: Int -> [Task] -> [Task]
removeTask _ [] = []
removeTask i (task:tasks)
    | i == 0 = tasks
    | otherwise = task : removeTask (i - 1) tasks

data Command
    = Exit_
    | ShowTasks
    | RemoveTask Int
    | AddTask Task
    deriving (Show)

makeProgram :: a -> IO a
makeProgram = return

type Handler = [Task] -> (String, Maybe [Task])

-- | Print back a list of current tasks.
--
-- >>> handleShowTasks ["task 1", "task 2"]
-- ("task 1\ntask 2\n",Just ["task 1","task 2"])
handleShowTasks :: Handler
handleShowTasks tasks = (feedback, Just tasks)
    where
        feedback = tasksToString tasks

handleExit_ :: Handler
handleExit_ _tasks = ("Bye!", Nothing)

handleRemoveTask :: Int -> Handler
handleRemoveTask i tasks = ("Task removed.", Just newTasks)
    where
        newTasks = removeTask i tasks

handleAddTask :: Task -> Handler
handleAddTask newTask tasks = (feedback, Just newTasks)
    where
        feedback = "New task: " ++ newTask
        newTasks = newTask : tasks

handleCommand :: Command -> Handler
handleCommand command = case command of
  Exit_ -> handleExit_
  ShowTasks -> handleShowTasks
  RemoveTask taskIndex -> handleRemoveTask taskIndex
  AddTask newTask -> handleAddTask newTask
        
-- | Parse a task manager bot command.
--
-- >>> parseCommand "/done 3"
-- Just (RemoveTask 3)
-- >>> parseCommand "/done 1 2 3"
-- Nothing
parseCommand :: String -> Maybe Command
parseCommand input =
    case input of
        "/exit" -> Just Exit_
        "/show" -> Just ShowTasks
        _ ->
            case words input of
                ["/done", indexStr] -> 
                    case readMaybe indexStr of
                        Nothing -> Nothing
                        Just i -> Just (RemoveTask i)
                "/done":_ -> Nothing
                _ -> Just (AddTask input)

-- | Default entry point.
run :: IO ()
run = runWith
    ["buy milk", "grade hw"]
    parseCommand
    handleCommand

runWith
    :: state
    -> (String -> Maybe command)
    -> (command -> state -> (String, Maybe state))
    -> IO () 
runWith tasks parse handle = do
    putStr "command> "
    input <- getLine
    case parse input of
        Nothing -> do
            putStrLn "ERROR: unrecognized command"
            runWith tasks parse handle
        Just command' -> do
            case handle command' tasks of
                (feedback, newTasks) -> do
                    putStrLn feedback
                    case newTasks of
                        Nothing -> return ()
                        Just newTasks' -> do
                            runWith newTasks' parse handle
