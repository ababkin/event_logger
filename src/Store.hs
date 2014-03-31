{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Store where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Aeson (encode, decode)
import Control.Lens

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Notification

data Config = Config {
    _dbname   :: String
  , _user     :: String
  , _password :: String
  }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  NotificationRecord
    sourceType  String
    eventType   String
    payload     String
    timestamp   String
    start       Int
    duration    Int
    deriving Show

|]

connStr = "host=localhost dbname=event_logger user=gust port=5432"

migrate :: IO ()
migrate = withPostgresqlPool connStr 10 $ \pool -> do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll

saveNotification :: Notification -> IO ()
saveNotification n = withPostgresqlPool connStr 10 $ \pool -> do
  flip runSqlPersistMPool pool $ do
    let notificationRecord = toNotificationRecord n
    liftIO $ putStrLn $ "inserting: " ++ (show notificationRecord)
    notificationId <- insert notificationRecord
    return ()

    {- insert $ BlogPost "My fr1st p0st" johnId -}
    {- insert $ BlogPost "One more for good measure" johnId -}

    {- oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1] -}
    {- liftIO $ print (oneJohnPost :: [Entity BlogPost]) -}

    {- john <- get johnId -}
    {- liftIO $ print (john :: Maybe Person) -}

    {- delete janeId -}
    {- deleteWhere [BlogPostAuthorId ==. johnId] -}

toNotificationRecord :: Notification -> NotificationRecord
toNotificationRecord n = NotificationRecord
                          (T.unpack $ n^.sourceType)
                          (show $ n^.eventType)
                          (BL.unpack $ encode $ n^.payload)
                          (T.unpack $ n^.timestamp)
                          (n^.start)
                          (n^.duration)
