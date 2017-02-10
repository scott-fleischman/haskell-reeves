{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Byte
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified System.Process as Process
import qualified System.Posix.Env.ByteString as Byte

parseJson
  :: String
  -> (HashMap Text (HashMap Text Text) -> IO ())
  -> IO ()
parseJson file f = do
  contents <- Byte.readFile file
  case Aeson.decodeStrict' contents of
    Nothing -> putStrLn "Unable to read file"
    Just top -> f top

executeOrIgnore :: Text -> HashMap Text Text -> IO ()
executeOrIgnore m t = case HashMap.lookup m t of
  Nothing -> return ()
  Just s -> do
    let ss = Text.unpack s
    putStrLn $ "> " ++ ss
    code <- Process.runCommand ss >>= Process.waitForProcess
    putStrLn $ show code
    putStrLn ""

main :: IO ()
main = parseJson "reeves.json" $ \reeves -> do
  (fmap . fmap) decodeUtf8 Byte.getArgs >>= \case
    ["-a", s] -> mapM_ (executeOrIgnore s) reeves
    _ -> putStrLn "Invalid arguments: use `reeves -a <cmd>` or `reeves <name> <cmd>`"
