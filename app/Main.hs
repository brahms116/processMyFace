module Main (main) where

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (find)
import Data.Maybe (isJust)
import System.Environment
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

runValidateTask :: Task -> ExceptT String (StateT [RunningTask] IO) ()
runValidateTask t = do
  ts <- lift get
  case validateTask t ts of
    Left err -> throwError err
    Right _ -> return ()

addTaskToState :: (Monad a) => Task -> String -> ProcessHandle -> StateT [RunningTask] a ()
addTaskToState (Task name command d) fp ph =
  let rt = RunningTask {rtFp = fp, rtPh = ph, rtName = name, rtCommand = command, rtCwd = d}
   in do
        -- explore why a has to be a monad?
        cur <- get
        put $ rt : cur

addTask :: Task -> ExceptT String (StateT [RunningTask] IO) String
addTask t =
  let doIO = lift . lift
      command = shell (tCommand t)
      filename = "/tmp/" ++ tName t
   in do
        _ <- runValidateTask t
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
        _ <- lift $ addTaskToState t filename ph
        return $ "Added task: " ++ tName t

showLogs :: RunningTask -> IO ()
showLogs (RunningTask _ _ _ _ p) =
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

runAction :: Action -> StateT [RunningTask] IO ()
runAction (AddTask t) = do
  result <- runExceptT $ addTask t
  case result of
    Left s -> lift $ putStrLn s
    Right s -> lift $ putStrLn s
runAction (LogTask n) =
  let f x = rtName x == n
   in do
        mTask <- find f <$> get
        case mTask of
          Nothing -> lift $ putStrLn "No such task!"
          Just ts -> lift $ showLogs ts

loop :: StateT [RunningTask] IO ()
loop =
  menu >>= runAction >> loop

main :: IO ()
main = void $ runStateT loop []

-- main = do
-- args <- pTask <$> getArgs
-- hFile <- openFile ("/tmp/" ++ tName args) WriteMode
-- hSetBuffering hFile NoBuffering
-- hPutStr hFile ""
-- (_, Just mout, _, _) <-
--   createProcess
--     (shell (tCommand args))
--       { cwd = Just $ tCwd args,
--         std_in = CreatePipe,
--         std_out = CreatePipe,
--         delegate_ctlc = False
--       }
-- contents <- hGetContents mout
-- hPutStr hFile contents
-- (_, _, _, _) <-
--   createProcess
--     -- (shell (tCommand args ++ " > /tmp/" ++ tName args ++ " 2>&1"))
--     (shell (tCommand args))
--       { cwd = Just $ tCwd args,
--         std_out = UseHandle hFile,
--         std_err = UseHandle hFile
--       }
-- (_, Just mout, _, _) <-
--   createProcess
--     (shell ("tail -f /tmp/" ++ tName args))
--       { std_out = CreatePipe,
--         std_in = CreatePipe,
--         delegate_ctlc = False
--       }
-- loop mout

-- loop :: Handle -> IO ()
-- loop h = do
--   isReady <- hReady h
--   threadDelay 100000
--   putStrLn "check"
--   if isReady
--     then recurse
--     else loop h
--   where
--     recurse = do
--       line <- hGetLine h
--       putStrLn line
--       loop h
