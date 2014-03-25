{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Notification where

import Control.Applicative
import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

{- import qualified Data.ByteString      as BS -}

          {- packet_hash = { -}
            {- source_type: 'rails', -}
            {- event_type: name, -}
            {- payload: payload, -}
            {- timestamp: start.strftime('%H:%M:%S:(%L)'), -}
            {- start_milliseconds: (start.to_f * 1000.0).to_i, -}
            {- duration: ((finish - start) * 1000.0).to_i -}
          {- } -}


data EventType = 
    ControllerProcessStart
  | ControllerProcessFinish
  | Sql
  | RenderTemplate
  | RenderPartial
  | UnknownEventType
  deriving Show

data PayloadType = 
    ControllerPayloadType
  | SqlPayloadType
  | RenderPayloadType
  | UnknownPayloadType

data Payload = 
  RenderPayload{
    pIdentifier  :: T.Text
  }
  |
  SqlPayload{
    pSql         :: T.Text
  }
  |
  ControllerPayload{
    pController  :: T.Text
  , pAction      :: T.Text
  } 
  |
  UnknownPayload
  deriving (Generic, Show)

instance ToJSON Payload


data Notification = Notification{
    nSourceType :: T.Text
  , nEventType  :: EventType
  , nPayload    :: Payload
  , nTimestamp  :: T.Text
  , nStart      :: Int
  , nDuration   :: Int
  } deriving (Show)


instance FromJSON Notification where
  parseJSON (Object v) = do
    eventType <- getEventType <$> v .: "event_type"
    payload <- v .: "payload"
    specificPayload <- case getPayloadType eventType of
      RenderPayloadType     -> RenderPayload      <$> payload .: "identifier"
      SqlPayloadType        -> SqlPayload         <$> payload .: "sql"
      ControllerPayloadType -> ControllerPayload  <$> payload .: "controller"
                                                  <*> payload .: "action"
      _ -> return UnknownPayload

    sourceType  <- v .: "source_type"
    timeStamp   <- v .: "timestamp"
    start       <- v .: "start_milliseconds"
    duration    <- v .: "duration"

    return $ Notification sourceType eventType specificPayload timeStamp start duration

    where
      getPayloadType :: EventType -> PayloadType
      getPayloadType et = case et of
        ControllerProcessStart  -> ControllerPayloadType
        ControllerProcessFinish -> ControllerPayloadType
        Sql                     -> SqlPayloadType
        RenderTemplate          -> RenderPayloadType
        RenderPartial           -> RenderPayloadType
        UnknownEventType        -> UnknownPayloadType

      getEventType :: T.Text -> EventType
      getEventType ett = case ett of
        "start_processing.action_controller"  -> ControllerProcessStart
        "process_action.action_controller"    -> ControllerProcessFinish
        "sql.active_record"                   -> Sql
        {- "!render_template.action_view"        -> RenderTemplate -}
        "render_template.action_view"         -> RenderTemplate
        "render_partial.action_view"          -> RenderPartial
        _                                     -> UnknownEventType
      


