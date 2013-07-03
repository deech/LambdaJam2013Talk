{-# LANGUAGE ScopedTypeVariables #-}
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Exception
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.State
import "mtl" Control.Monad.RWS
import Data.Char
import Debug.Trace

get_users :: IO [(String,String)]
get_users = do
  rows :: [[SqlValue]] <-
        dbQuery "select * from users" []
  let marshalled =
          map (\(user:pass:[]) ->
               (fromSql user, fromSql pass))
              rows
  return marshalled
  where
    dbConnect = connectSqlite3 "userdb.sqlite"
    dbQuery sql values =
          bracket dbConnect disconnect
            (\conn -> quickQuery' conn sql values)

simple_auth :: (String,String) -> Reader [(String,String)] Bool
simple_auth (user,pass) = do
  users :: [(String,String)] <- ask
  case (lookup user users) of
    Nothing -> return False
    Just p -> return (p == pass)

interactive_auth =
  let puts     msg = liftIO (putStrLn msg)
      wait_for msg = do {puts msg; liftIO getLine}
      log_failed   = tell ["Failed login attempt"]
      set_user u   = do {puts "Welcome!"; put u}
  in do users    <- ask
        user     <- wait_for "Username:"
        password <- wait_for "Password:"
        case (lookup user users) of
          Nothing -> do puts "Invalid Login!"
                        log_failed
          Just p  -> if (p == password)
                     then set_user user
                     else log_failed

simple_auth_driver =
    let my_auth = ("deech","deechpassword") in
    do users :: [(String,String)] <- get_users
       print (runReader (simple_auth my_auth) users)

interactive_auth_driver = do
    let my_auth = ("deech","deechpassword")
    users <- get_users
    let writer = runReaderT interactive_auth users
    let state  = runWriterT writer
    let io     = runStateT state ""
    final <- io
    print final

interactive_auth_driver' = do
    let my_auth = ("deech","deechpassword")
    users <- get_users
    final <- runRWST interactive_auth users ""
    print final

dual_states :: ReaderT String (WriterT [String] (StateT Int (StateT String IO))) ()
dual_states = do
  ask
  tell ["help"]
  lift $ lift $ put 1
  lift $ lift $ lift $ put "hello world"

data Auth_State = Auth_State {counter :: Int, current_user :: String} deriving Show
increment_attempt_counter = do
  auth_state <- get
  put auth_state{counter = (counter auth_state + 1)}
