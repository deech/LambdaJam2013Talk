{-# LANGUAGE ScopedTypeVariables #-}
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Exception
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import Data.Char
import Debug.Trace
get_users :: IO [(String,String)]
get_users = do
  rows :: [[SqlValue]] <-
        dbQuery "select * from users" []
  let marshalled :: [(String,String)] =
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
main =
    let my_auth = ("deech","deechpassword") in
    do users :: [(String,String)] <- get_users
       print (runReader (simple_auth my_auth) users)
