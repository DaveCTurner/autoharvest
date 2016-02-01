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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options

daysFrom :: Day -> [Day]
daysFrom = filter isWeekDay . iterate (addDays 1)
  where
  isWeekDay (ModifiedJulianDay n) = mod n 7 `elem` [0,1,2,5,6]

data TimePeriod = TimePeriod
  { startedAt :: String
  , endedAt :: String
  , spentAt :: Day
  }

instance ToJSON TimePeriod where
  toJSON TimePeriod{..} = object
    [ "notes" .= ("" :: String)
    , "started_at" .= startedAt
    , "ended_at"   .= endedAt
    , "project_id" .= (9547486 :: Int)
    , "task_id"    .= (4210505 :: Int)
    , "spent_at"   .= show spentAt
    ]

main :: IO ()
main = withOptions $ \Config{..} -> do
  password <- getEnv "HARVEST_PASSWORD"
  manager <- newManager tlsManagerSettings

  let req = (applyBasicAuth "david.turner@tracsis.com"
                            $ T.encodeUtf8 $ T.pack password)
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
            }
          currentReq = req { requestBody = RequestBodyLBS $ encode timePeriod }

      void $ httpNoBody currentReq manager
