module Main where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Byte
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

handleInput
  :: String
  -> (HashMap Text (HashMap Text Text) -> IO ())
  -> IO ()
handleInput file f = do
  contents <- Byte.readFile file
  case Aeson.decodeStrict' contents of
    Nothing -> putStrLn "Unable to read file"
    Just top -> f top

main :: IO ()
main = handleInput "example.json" (\_ -> putStrLn "Success")
