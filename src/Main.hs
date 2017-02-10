module Main where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Byte
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
  contents <- Byte.readFile "example.json"
  case Aeson.decodeStrict' contents :: Maybe (HashMap Text (HashMap Text Text)) of
    Nothing -> putStrLn "Unable to read file"
    Just top -> putStrLn "Success"
