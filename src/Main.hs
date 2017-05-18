{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (_String, key, values)
import qualified Data.Char as Char
import Data.Text hiding (take)
import qualified Data.Text.IO as T (putStrLn)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml
import Network.HTTP.Client hiding (checkResponse, responseBody, responseStatus)
import Network.HTTP.Types.Status (Status(..), status403, status404)
import Network.Wreq
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath (joinPath)

apiBaseUrl = "https://od-api.oxforddictionaries.com:443/api/v1/entries/en/"
language = "en"

toWordId :: String -> String
toWordId = fmap Char.toLower

getOptions :: AppConfig -> Options
getOptions appConfig = defaults
  & set (header "accept") ["application/json"]
  & set (header "app_key") [encodeUtf8 (apiKey appConfig)]
  & set (header "app_id") [encodeUtf8 (appId appConfig)]
  & set checkResponse (Just responseChecker)
    where
      -- Prevents an exception being thrown on a non 'ok' status
      responseChecker :: Request -> (Response a) -> IO ()
      responseChecker req res = return ()

urlForWordDefinitions :: String -> String
urlForWordDefinitions word = apiBaseUrl ++ (toWordId word) ++ "/definitions"

getWordDefinition :: Options -> String -> IO (Status, Maybe Text)
getWordDefinition opts word = do
  response <- getWith opts (urlForWordDefinitions word)
  let status = response ^. responseStatus
  let wordDef = response ^? responseBody
        . key "results" . values
        . key "lexicalEntries" . values
        . key "entries" . values
        . key "senses" . values
        . key "definitions" . values
        . _String
  return $ (status, wordDef)

data AppConfig = AppConfig { apiKey :: Text, appId :: Text } deriving Show
instance FromJSON AppConfig where
  parseJSON (Object m) = AppConfig <$> m .: "apiKey" <*> m .: "appId"
  parseJSON x = fail $ show x

configFilePath :: IO FilePath
configFilePath = getHomeDirectory >>=
  \homeDir -> return $ joinPath [homeDir, ".define.yaml"]

readConfigFile :: IO (Either ParseException AppConfig)
readConfigFile =
  decodeFileEither =<< configFilePath

main :: IO ()
main = do
  words <- fmap (take 1) getArgs
  case words of
    [] -> putStrLn "usage: define <word>"
    word:_ -> do
      config <- readConfigFile
      case config of
        Left (InvalidYaml (Just (YamlException msg))) -> putStrLn $ "Missing config gile" ++ msg
        Left (InvalidYaml (Just (YamlParseException _ _ (YamlMark _ line column)))) -> putStrLn $ "Invalid config file, check your formatting at line: " ++ show line ++ " column: " ++ show column
        Left (AesonException msg) -> putStrLn $ "Invalid config gile " ++ msg
        Left x -> putStrLn $ show x
        Right appConfig -> do
          definitionResponse <- getWordDefinition (getOptions appConfig) word
          case definitionResponse of
            (Status 200 _, Just definition) ->  T.putStrLn definition
            (Status 403 _, _) ->  putStrLn "Bad credentials, check your .define file"
            (Status 404 _, _) ->  putStrLn "No definition found"
            otherwise ->  print definitionResponse
