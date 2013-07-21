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
        "!remove" -> return $"PRIVMSG " ++ chan ++ " :" ++ removE
        "!read" -> return $"PRIVMSG " ++ chan ++ " :" ++ reaD
        otherwise -> return ""
    else return ""
  where command = words msg
        action = tail $ command !! 3
        chan =  command !! 2
        server = unwords . tail $ command
        user = tail $takeWhile  (/='!') $head command
        message = unwords $drop 4 command


reaD = "plugin not complete"
adD :: (IConnection a) => String -> String -> a -> IO String
adD user message conn = do
  r <- (run conn "INSERT INTO todo VALUES (?,?)" [toSql user,toSql message])
  commit conn
  if r == 0
    then return $user++": Sucsess fully added todo"
    else return $user++": Something has gone wrong :("
  --where
--    r<-conn "INSERT INTO todo VALUES (?,?)" [toSql user,toSql msg]) 
removE = "plugin not complete"
