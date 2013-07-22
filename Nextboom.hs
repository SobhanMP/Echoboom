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
        otherwise -> return ""
    else return ""
  where command = words msg
        action = tail $ command !! 3
        chan =  command !! 2
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
         
removE :: (IConnection a) => String -> String -> a -> IO String
removE user "" conn = return ("action not defined")
removE user n conn = do
  r <-  reaD' user n conn
  print user
  print r
  a <-  run conn ("delete from todo where name=? and msg=?") [toSql user,toSql r]
  return ("removed todo")
