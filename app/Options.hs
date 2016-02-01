module Options
  ( Config(..)
  , withOptions
  ) where

import Control.Applicative
import Data.Monoid
import Options.Applicative
import Data.Time

data Config = Config
  { cStartDate :: Day
  , cDayCount  :: Int
  } deriving (Show, Eq)

optionsApplicative :: Parser Config
optionsApplicative = Config
  <$> option auto   (long "start-date"
                  <> metavar "YYYY-MM-DD"
                  <> help "First day to fill in")
  <*> option auto   (long "days"
                  <> metavar "DAYS"
                  <> help "Number of (work) days to fill in")

optionsInfo :: ParserInfo Config
optionsInfo
  = info (helper <*> optionsApplicative)
         (fullDesc <> progDesc "Automatic timesheet filler-inner")

withOptions :: (Config -> IO ()) -> IO ()
withOptions go = execParser optionsInfo >>= go
