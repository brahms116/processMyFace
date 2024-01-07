{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (traverse_)
import Data.List (find, transpose)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess)
import System.IO
import System.Process

data Task = Task
  { tName :: String,
    tCommand :: String,
    tCwd :: String
  }
  deriving (Show)

data RunningTask = RunningTask
  { rtName :: String,
    rtCommand :: String,
    rtCwd :: String,
    rtPh :: ProcessHandle,
    rtFp :: String
  }

data JsonTask = JsonTask String String (Maybe String)

newtype JsonTasks = JsonTasks [JsonTask]

instance FromJSON JsonTask where
  parseJSON = withObject "JsonTask" $ \v -> do
    name <- v .: "name"
    command <- v .: "command"
    d <- v .:? "cwd"
    return $ JsonTask name command d

instance FromJSON JsonTasks where
  parseJSON = withObject "JsonTasks" $ \v -> do
    tasks <- v .: "tasks"
    return $ JsonTasks tasks

instance Show RunningTask where
  show = rtName

data Action = AddTask Task | LogTask String | Kill String | Exit deriving (Show)

type ApplicationContext = StateT [RunningTask] IO

prettyTable :: [[String]] -> String
prettyTable ss =
  let ss' = transpose ss
      widths = (maximum . (length <$>) <$> ss')
      fixed = [[x ++ replicate (i + 4 - length x) ' ' | (x, i) <- zip y widths] | y <- ss]
   in unlines $ unwords <$> fixed

pTask :: String -> Either String Task
pTask s = case words s of
  [name, cmd, d] -> Right $ Task {tName = name, tCommand = cmd, tCwd = d}
  _ -> Left $ "Error parsing task: " ++ s

pAction :: String -> Either String Action
pAction s =
  case words s of
    ("add" : ws) -> do
      t <- pTask $ unwords ws
      return $ AddTask t
    -- better parse name here
    ("logs" : name : _) -> Right $ LogTask name
    ("kill" : name : _) -> Right $ Kill name
    ("exit" : _) -> Right Exit
    _ -> Left "Invalid command"

promptText :: String
promptText =
  unlines
    [ "Available commands:",
      "======================",
      "- 'logs <TASK_NAME>', see the live tail of logs from <TASK_NAME>, press 'q' then to return to this menu",
      "- 'kill <TASK_NAME>', kill the task <TASK_NAME> by sending SIGTERM",
      "- 'exit', kill all child processes and exit",
      "",
      "",
      "Enter your command, then hit <ENTER>"
    ]

prompt :: IO Action
prompt =
  putStrLn promptText >> pAction <$> getLine >>= \case
    Right action -> return action
    Left str -> putStrLn str >> prompt

prettyMenu :: [Maybe ExitCode] -> [RunningTask] -> String
prettyMenu exitCodes ts =
  unlines
    [ "",
      "",
      "",
      "Running Tasks",
      "=============="
    ]
    ++ prettyTable taskTable
  where
    taskTable =
      ["Name:", "Command:", "Directory:", "Log file path", "Status"]
        : (uncurry taskTableRow <$> zip ts exitCodes)

    taskTableRow :: RunningTask -> Maybe ExitCode -> [String]
    taskTableRow (RunningTask name cmd d _ lp) code =
      [ name,
        cmd,
        d,
        lp,
        maybe "RUNNING" (\x -> "[EXITED with code " ++ exitCodeStr x ++ "]") code
      ]

exitCodeStr :: ExitCode -> String
exitCodeStr ExitSuccess = "0"
exitCodeStr (ExitFailure c) = show c

menu :: StateT [RunningTask] IO Action
menu = do
  exitCodes <- gets (map rtPh) >>= lift . traverse getProcessExitCode
  (get >>= lift . putStrLn . prettyMenu exitCodes) >> lift prompt

validateTask :: Task -> [RunningTask] -> Either String ()
validateTask t ts =
  let name = tName t
      dup = any (\x -> name == rtName x) ts
   in when dup $ Left $ "Duplicate task name: " ++ name

validateTaskInCtx :: Task -> ExceptT String ApplicationContext ()
validateTaskInCtx t =
  lift get >>= \ts -> case validateTask t ts of
    Left err -> throwError err
    Right _ -> return ()

addTaskToCtx :: Task -> String -> ProcessHandle -> ApplicationContext ()
addTaskToCtx (Task name command d) fp ph =
  let rt = RunningTask {rtFp = fp, rtPh = ph, rtName = name, rtCommand = command, rtCwd = d}
   in get >>= put . (rt :)

addTask :: Task -> ExceptT String ApplicationContext String
addTask t =
  let doIO = lift . lift
      command = shell (tCommand t)
      filename = "/tmp/" ++ tName t
   in do
        _ <- validateTaskInCtx t
        hFile <- doIO $ openFile filename WriteMode
        -- dunno if I need to set the buffering mode
        (_, _, _, ph) <-
          doIO $
            createProcess
              command
                { cwd = Just $ tCwd t,
                  std_in = CreatePipe,
                  delegate_ctlc = False,
                  std_out = UseHandle hFile,
                  std_err = UseHandle hFile
                }
        _ <- lift $ addTaskToCtx t filename ph
        return $ "Added task: " ++ tName t

findTaskRun :: (RunningTask -> ApplicationContext ()) -> String -> ExceptT String ApplicationContext ()
findTaskRun toRun name =
  let f x = rtName x == name
   in lift get >>= \ts -> case find f ts of
        Nothing -> throwError $ "No such task: " ++ name
        Just t -> lift $ toRun t

showLogs :: String -> ExceptT String ApplicationContext ()
showLogs = findTaskRun $ lift . showLogForTask

showLogForTask :: RunningTask -> IO ()
showLogForTask (RunningTask _ _ _ _ p) =
  let cmd = shell ("tail -f  -n +1 " ++ p)
   in do
        (_, Just mout, _, ph) <-
          createProcess
            cmd
              { std_out = CreatePipe,
                delegate_ctlc = False,
                std_in = CreatePipe
              }
        mVar <- newEmptyMVar
        _ <- forkIO $ waitForQ mVar
        _ <- logLoop mout mVar
        terminateProcess ph

killTask :: String -> ExceptT String ApplicationContext ()
killTask = findTaskRun $ \t -> lift $ do
  code <-
    terminateProcess (rtPh t)
      >> threadDelay 1000
      >> getProcessExitCode (rtPh t)
  case code of
    Nothing -> putStrLn "Failed to kill process"
    Just c -> putStrLn $ "Exited with code " ++ exitCodeStr c

waitForQ :: MVar Bool -> IO ()
waitForQ mVar =
  hSetBuffering stdin NoBuffering
    >> getChar
    >>= \c ->
      if c == 'q'
        then hSetBuffering stdin (BlockBuffering Nothing) >> putMVar mVar True
        else waitForQ mVar

logLoop :: Handle -> MVar Bool -> IO ()
logLoop h m = do
  quit' <- isJust <$> tryTakeMVar m
  if quit'
    then return ()
    else do
      isReady <- hReady h
      threadDelay 100
      if isReady
        then hGetLine h >>= putStrLn >> logLoop h m
        else logLoop h m

runAction :: Action -> ApplicationContext ()
runAction (AddTask t) = do
  result <- runExceptT $ addTask t
  case result of
    Left s -> lift $ putStrLn s
    Right _ -> return ()
runAction (LogTask n) = do
  runExceptT (showLogs n) >>= \case
    Left s -> lift $ putStrLn s
    Right _ -> return ()
runAction (Kill n) = do
  runExceptT (killTask n) >>= \case
    Left s -> lift $ putStrLn s
    Right _ -> return ()
runAction Exit = do
  runningTasks <- get
  lift $ traverse_ (terminateProcess . rtPh) runningTasks >> threadDelay 1000
  running <- lift $ traverse namePidPair runningTasks
  case filter (isJust . snd) running of
    [] -> pure ()
    ts -> do
      lift $ putStrLn "\nFAILED TO KILL:"
      lift . putStrLn . prettyTable $
        (\(name, pid) -> [name, maybe "" show pid]) <$> ts
  lift exitSuccess
  where
    namePidPair task = (,) (rtName task) <$> getPid (rtPh task)

parseJsonTasks :: String -> Either String JsonTasks
parseJsonTasks = eitherDecode . pack

getDefinedTasks :: String -> MaybeT IO JsonTasks
getDefinedTasks p = MaybeT $ do
  r <- parseJsonTasks <$> readFile p
  case r of
    Left str -> putStrLn str >> return Nothing
    Right ts -> return $ Just ts

tasksFromArg :: MaybeT IO JsonTasks
tasksFromArg = do
  args <- lift getArgs
  case args of
    (p : _) -> getDefinedTasks p
    _ -> MaybeT $ return Nothing

loop :: ApplicationContext ()
loop = (menu >>= runAction) >> loop

taskFromJsonTask :: JsonTask -> Task
taskFromJsonTask (JsonTask name cmd d) = Task name cmd $ fromMaybe "." d

app :: ApplicationContext ()
app =
  let handleResult x = case x of
        Left s -> putStrLn s
        Right _ -> pure ()
   in do
        JsonTasks ts <- lift $ fromMaybe (JsonTasks []) <$> runMaybeT tasksFromArg
        result <- runExceptT $ traverse (addTask . taskFromJsonTask) ts
        lift $ handleResult result
        loop

main :: IO ()
main = void $ runStateT app []
