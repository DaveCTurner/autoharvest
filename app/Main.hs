{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Method
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options

daysFrom :: Day -> [Day]
daysFrom = filter isWeekDay . iterate (addDays 1)
  where
  isWeekDay (ModifiedJulianDay n) = mod n 7 `elem` [0,1,2,5,6]

data TimePeriod = TimePeriod
  { startedAt :: String
  , endedAt   :: String
  , spentAt   :: Day
  , projectId :: Int
  , taskId    :: Int
  }

instance ToJSON TimePeriod where
  toJSON TimePeriod{..} = object
    [ "notes" .= ("" :: String)
    , "started_at" .= startedAt
    , "ended_at"   .= endedAt
    , "project_id" .= projectId
    , "task_id"    .= taskId
    , "spent_at"   .= show spentAt
    ]

getEnvBS :: String -> IO B.ByteString
getEnvBS = fmap (T.encodeUtf8 . T.pack) . getEnv

main :: IO ()
main = withOptions $ \Config{..} -> do
  username <- getEnvBS "HARVEST_USER"
  password <- getEnvBS "HARVEST_PASSWORD"
  manager <- newManager tlsManagerSettings

  let req = applyBasicAuth username password
            ((fromMaybe (error "could not make request")
             $ parseUrl "https://tracsis.harvestapp.com/daily/add")
        { requestHeaders =
            [ ("Content-type", "application/json")
            , ("Accept",       "application/json")
            ]
        , method = methodPost })

  forM_ (take cDayCount $ daysFrom cStartDate) $ \day ->
    forM_ [("09:00","12:30"),("13:00","17:30")] $ \(startTime, endTime) -> do
      let timePeriod = TimePeriod
            { startedAt = startTime
            , endedAt   = endTime
            , spentAt   = day
            , projectId = cProjectId
            , taskId    = cTaskId
            }
          currentReq = req { requestBody = RequestBodyLBS $ encode timePeriod }

      void $ httpNoBody currentReq manager
