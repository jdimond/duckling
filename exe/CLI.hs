{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS
import Data.Semigroup ((<>))
import Data.Time.LocalTime
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import Text.Read (readMaybe)

import Duckling.Core hiding (value)
import Duckling.Data.TimeZone


data Options = Options
    { inputLanguge :: String
    , timeZone :: String
    , rawInput :: String
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> (strOption $
        long "lang"
        <> short 'l'
        <> help "Language of input text"
        <> showDefault
        <> value "EN"
        <> metavar "LANG" )
    <*> (strOption $
        long "time-zone"
        <> short 'z'
        <> help "Time zone of input text"
        <> value defaultTimeZone
        <> metavar "TZ" )
    <*> (strArgument $ metavar "INPUT")

defaultTimeZone :: String
defaultTimeZone = "America/Los_Angeles"

main = do
    opts <- execParser cliOptions
    tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
    let inputText = Text.pack $ rawInput opts
        inputTimeZone = Text.pack $ timeZone opts
    refTime <- liftIO $ currentReftime tzs inputTimeZone
    let parsedResult = parse inputText (context refTime defaultLang) []
    BS.putStrLn $ encode parsedResult
  where
    cliOptions = info optionsParser $ progDesc "Execute Duckling from your CLI."
    defaultLang = EN
    context t l = Context
      { referenceTime = t
      , lang = l
      }
