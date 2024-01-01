module Main (main) where

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Data.List (find)
import Data.Maybe (isJust)
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

instance Show RunningTask where
  show = rtName

data Action = AddTask Task | LogTask String deriving (Show)

type ApplicationContext = StateT [RunningTask] IO

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
    ("log" : name : _) -> Right $ LogTask name
    _ -> Left "Invalid command"

prompt :: IO Action
prompt = do
  putStrLn "Enter your command: "
  a <- pAction <$> getLine
  case a of
    Right action -> return action
    Left str -> putStrLn str >> prompt

menu :: StateT [RunningTask] IO Action
menu = do
  cur <- get
  lift $ print cur
  lift prompt

validateTask :: Task -> [RunningTask] -> Either String ()
validateTask t ts =
  let name = tName t
      dup = any (\x -> name == rtName x) ts
   in when dup $ Left $ "Duplicate task name: " ++ name

validateTaskInCtx :: Task -> ExceptT String ApplicationContext ()
validateTaskInCtx t = do
  ts <- lift get
  case validateTask t ts of
    Left err -> throwError err
    Right _ -> return ()

addTaskToCtx :: Task -> String -> ProcessHandle -> ApplicationContext ()
addTaskToCtx (Task name command d) fp ph =
  let rt = RunningTask {rtFp = fp, rtPh = ph, rtName = name, rtCommand = command, rtCwd = d}
   in do
        cur <- get
        put $ rt : cur

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
                  std_out = UseHandle hFile,
                  std_err = UseHandle hFile
                }
        _ <- lift $ addTaskToCtx t filename ph
        return $ "Added task: " ++ tName t

showLogs :: String -> ExceptT String ApplicationContext ()
showLogs n = do
  ts <- lift get
  let f x = rtName x == n
  case find f ts of
    Nothing -> throwError $ "No such task: " ++ n
    Just ts' -> lift . lift $ showLogForTask ts'

showLogForTask :: RunningTask -> IO ()
showLogForTask (RunningTask _ _ _ _ p) =
  let cmd = shell ("tail -f " ++ p)
   in do
        (_, Just mout, _, ph) <-
          createProcess
            cmd
              { std_out = CreatePipe,
                delegate_ctlc = False
              }
        mVar <- newEmptyMVar
        _ <- forkIO $ waitForQ mVar
        _ <- logLoop mout mVar
        terminateProcess ph

waitForQ :: MVar Bool -> IO ()
waitForQ mVar = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  if c == 'q'
    then putMVar mVar True
    else waitForQ mVar

logLoop :: Handle -> MVar Bool -> IO ()
logLoop h m =
  let quit = isJust <$> tryTakeMVar m
   in do
        quit' <- quit
        if quit'
          then return ()
          else do
            isReady <- hReady h
            threadDelay 10000
            if isReady
              then recurse
              else logLoop h m
  where
    recurse = do
      line <- hGetLine h
      putStrLn line
      logLoop h m

runAction :: Action -> ApplicationContext ()
runAction (AddTask t) = do
  result <- runExceptT $ addTask t
  case result of
    Left s -> lift $ putStrLn s
    Right s -> lift $ putStrLn s
runAction (LogTask n) = do
  result <- runExceptT $ showLogs n
  case result of
    Left s -> lift $ putStrLn s
    Right _ -> return ()

loop :: ApplicationContext ()
loop =
  menu >>= runAction >> loop

main :: IO ()
main = void $ runStateT loop []
