module Nextboom
(
  pluginRun)
where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad

concatIOStrings' :: IO String -> IO String -> IO String
concatIOStrings' a b = do
  a' <- a
  b' <- b
  return $ a' ++ b'
concatIOStrings :: [IO String] -> IO String
concatIOStrings l = foldl concatIOStrings' (return "") l

pluginRun :: (IConnection a) => String -> a -> IO String
pluginRun msg conn=
  if head command == "PING"
  then return $ "PONG" ++ server
  else
    if length command >= 4
    then
      case action of
        "!add" -> concatIOStrings [return ("PRIVMSG " ++ chan ++ " :" ),adD user message conn]
        "!remove" ->  concatIOStrings [return $("PRIVMSG " ++ chan ++ " :"), removE user message conn]
        "!read" -> concatIOStrings [return ("PRIVMSG " ++ chan ++ " :"),reaD user message conn]
        "!add'" -> concatIOStrings [return ("PRIVMSG " ++ chan ++ " :" ),adDother user (command!!4) (unwords (drop 5 command )) conn]
        "!bulk" -> concatIOStrings [return ("PRIVMSG " ++ chan ++ " :" ),bulk bulk_num (unwords((take  3 command ) ++ (drop 5 command)))  conn]
        otherwise -> return ""
    else return ""
  where command = words msg
        bulk_num = read $ command !! 4 :: Int
        action = tail $ command !! 3
        chan = command !! 2
        server = unwords . tail $ command
        user = tail $takeWhile  (/='!') $head command
        message = unwords $drop 4 command

reaD :: (IConnection a) => String -> String -> a -> IO String
reaD user "" conn = do
  r <- (quickQuery' conn "select msg from todo where name =?" [toSql user])
  return (user ++ ": " ++ show(length r) ++ " undone todo")

reaD user msg conn = do
  let num = read msg :: Int
  r <-(quickQuery' conn "select msg from todo where name=?" [toSql user])
  concatIOStrings [return (user++": " ),return( fromSql  (r !! (read msg-1)!!0)::String)]

reaD' :: (IConnection a) => String -> String -> a ->IO String
reaD' user msg conn = do
  let num = read msg :: Int
  r <-(quickQuery' conn "select msg from todo where name=?" [toSql user])
  return  (fromSql (r !! (num -1) !!0) :: String)

adD :: (IConnection a) => String -> String -> a -> IO String
adD user message conn = do
  r <- (run conn "INSERT INTO todo VALUES (?,?)" [toSql user,toSql message])
  commit conn
  print r
  if r == 1
    then return $user++": Sucsess fully added todo"
    else return $user++": Something has gone wrong :("
adDother :: (IConnection a) => String -> String -> String -> a -> IO String
adDother user otheruser message conn = do
  r <- (run conn "INSERT INTO todo VALUES (?,?)" [toSql otheruser,toSql (user++ ": " ++ message)])
  commit conn
  print r
  if r == 1
    then return $user++": Sucsess fully added todo for "++otheruser
    else return $user++": Something has gone wrong :("
removE :: (IConnection a) => String -> String -> a -> IO String
removE user "" conn = return ("action not defined")
removE user n conn = do
  r <-  reaD' user n conn
  print user
  print r
  a <-  run conn ("delete from todo where name=? and msg=?") [toSql user,toSql r]
  commit conn
  return ("removed todo")
bulk :: (IConnection a) => Int-> String -> a -> IO String
bulk 0 msg conn = do
  return "muhahah1"
bulk n msg conn = do
  if n > 10
    then bulk 10 msg conn
    else do
    print n
    print msg
    commit conn
    concatIOStrings[pluginRun msg conn,return " ",bulk (n-1) msg conn]
  
