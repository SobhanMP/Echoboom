import Database.HDBC
import Database.HDBC.Sqlite3

pluginRun :: String -> String
pluginRun msg =
  if head command == "PING"
  then "PONG " ++ unwords . tail command
  else
    case action of
      "!add" -> adD
      "!remove" -> removE
      "!read" -> reaD
  where command = words msg
        action = head $ command !! 4


reaD = "plugin not complete"
adD = "plugin not complete"
removE = "plugin not complete"
--so that we don't get disconnected
    
    --  let user = tail . (takeWhile (/=!)) . head command)
--tail to remove the : in the front of the IRC command
--take while to remove the discription sobhan!~~sobhan@2.176.20.194
      --let chan =  command !! 2
      --let acction = init $ command !! 3
      
-- init to remove the first :

