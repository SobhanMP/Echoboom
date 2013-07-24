import Control.Exception
import Network
import System.IO
import Text.Printf
import Nextboom
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Exception
import Data.Either
server = "irc.freenode.org"
port   = 6667
chan   = ["#echoboom"]
nick   = "echo-boom-bot"
         
main = do
  conn <- connectSqlite3 "todo.db"
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick++" 0 * :tutorial bot")
  mapM (write h "JOIN") chan
  listen h conn
  disconnect conn

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

write' :: Handle -> String -> IO()
write' h s = do
  hPrintf h "%s \r\n" s 
  printf    "> %s \n" s

listen :: (IConnection a) => Handle -> a ->IO ()
listen h conn= forever $ do
       s <- hGetLine h
       msg <- pluginRun s conn
       commit conn
       when (msg /= "" )$write' h msg
      {- msg =<< try (pluginRun s conn) :: IO (Either SomeException String )
       case msg of
        (Right x) -> when (x /= "") $write' h x
        (Left err) -> print err -}
        
  where
      forever a = do a; forever a
