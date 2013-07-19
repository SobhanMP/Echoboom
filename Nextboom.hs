module Nextboom
(
  pluginRun)
where
import Database.HDBC
import Database.HDBC.Sqlite3

pluginRun :: String -> String
pluginRun msg =
  if head command == "PING"
  then "PONG " ++( unwords . tail command)
  else
    case action of
      "!add" -> "PRIVMSG " ++ chan ++ ':' ++ adD
      "!remove" -> "PRIVMSG " ++ chan ++ ':' ++ removE
      "!read" -> "PRIVMSG " ++ chan ++ ':' ++ reaD
      otherwise -> ""
  where command = words msg
        action = head $ command !! 4
        chan = command !! 3


reaD = "plugin not complete"
adD = "plugin not complete"
removE = "plugin not complete"
