module Nextboom
(
  pluginRun)
where
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
pluginRun :: String -> String
pluginRun msg =
  if head command == "PING"
  then "PONG " ++ server
  else
    if length command >= 4
    then
      case action of
        "!add" -> "PRIVMSG " ++ chan ++ " :" ++ adD
        "!remove" -> "PRIVMSG " ++ chan ++ " :" ++ removE
        "!read" -> "PRIVMSG " ++ chan ++ " :" ++ reaD
        otherwise -> ""
    else ""
  where command = words msg
        action = tail $ command !! 3
        chan =  command !! 2
        server = unwords . tail $ command


reaD = "plugin not complete"
adD = "plugin not complete"
removE = "plugin not complete"
