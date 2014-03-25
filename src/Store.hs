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
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Aeson (encode)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Notification

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

saveNotification :: Notification -> IO ()
saveNotification n = runSqlite ":memory:" $ do
  runMigration migrateAll
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
                          (T.unpack $ nSourceType n)
                          (show $ nEventType n)
                          (BL.unpack $ encode $ nPayload n)
                          (T.unpack $ nTimestamp n)
                          (nStart n)
                          (nDuration n)
