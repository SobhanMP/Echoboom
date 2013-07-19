module Nextboom
(
  pluginRun)
where
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
pluginRun :: (IConnection a) => String -> a -> String
pluginRun msg conn=
  if head command == "PING"
  then "PONG " ++ server
  else
    if length command >= 4
    then
      case action of
        "!add" -> "PRIVMSG " ++ chan ++ " :" ++ adD user message conn 
        "!remove" -> "PRIVMSG " ++ chan ++ " :" ++ removE 
        "!read" -> "PRIVMSG " ++ chan ++ " :" ++ reaD 
        otherwise -> ""
    else ""
  where command = words msg
        action = tail $ command !! 3
        chan =  command !! 2
        server = unwords . tail $ command
        user = tail $takeWhile  (/='!') $head command
        message = unwords $drop 4 command


reaD = "plugin not complete"
adD :: (IConnection a) => String -> String -> a -> String
adD user message conn = do
  r <- (run conn "INSERT INTO todo VALUES (?,?)" [toSql user,toSql message])
  commit conn
  if ((fromSql r) == 0 )
    then user++": Sucsess fully added todo"
    else user++": Something has gone wrong :("
  --where
--    r<-conn "INSERT INTO todo VALUES (?,?)" [toSql user,toSql msg]) 
removE = "plugin not complete"
