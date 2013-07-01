{-# LANGUAGE ScopedTypeVariables #-}
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Exception
testdb user_id = do
  res :: [[SqlValue]]  <- dbQuery
                           "select * from users where UserId=?"
                           [toSql user_id] :: IO [[SqlValue]]
  return res
  where
    dbConnect :: IO Connection
    dbConnect = connectSqlite3 "userdb.sqlite"
    dbQuery :: String -> [SqlValue] -> IO [[SqlValue]]
    dbQuery sql values =
          bracket dbConnect disconnect
            (\conn -> quickQuery' conn sql values)
