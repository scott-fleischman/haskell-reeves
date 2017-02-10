{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Byte
import           Data.Ord (comparing)
import qualified Data.List as List
import           Data.Maybe (maybe, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified System.Process as Process
import qualified System.Posix.Env.ByteString as Byte

data Top = Top
  { topCommands :: HashMap Text (HashMap Text Text)
  , topGroups :: HashMap Text [Text]
  }
instance Aeson.FromJSON Top where
  parseJSON (Aeson.Object v)
    = Top
    <$> v .: "commands"
    <*> v .: "groups"
  parseJSON _ = fail "Invalid JSON"

parseJson
  :: String
  -> (Top -> IO ())
  -> IO ()
parseJson file f = do
  contents <- Byte.readFile file
  case Aeson.decodeStrict' contents of
    Nothing -> putStrLn $ "Unable to load " ++ file
    Just top -> f top

executeOrIgnore :: Text -> HashMap Text Text -> IO ()
executeOrIgnore t m = case HashMap.lookup t m of
  Nothing -> return ()
  Just s -> do
    let ss = Text.unpack s
    putStrLn $ "> " ++ ss
    code <- Process.runCommand ss >>= Process.waitForProcess
    putStrLn $ show code
    putStrLn ""

printCommands :: Text -> (Text, HashMap Text Text) -> IO ()
printCommands pre (name, m) = putStrLn . Text.unpack . Text.concat $ [pre, name, "\t", Text.intercalate ", " . List.sort . HashMap.keys $ m]

printGroups :: Text -> HashMap Text [Text] -> IO ()
printGroups pre m = mapM_ printGroupValues (List.sortBy (comparing fst) . HashMap.toList $ m)
  where
  printGroupValues (x, y) = putStrLn . Text.unpack $ Text.concat [pre, x, "\t", Text.intercalate ", " . List.sort $ y]

doGroup :: Top -> Text -> Text -> IO ()
doGroup top g s = case HashMap.lookup g (topGroups top) of
  Nothing -> return ()
  Just ns -> do
    let allCmds = topCommands top
    let groupCmds = catMaybes . fmap (flip HashMap.lookup allCmds) $ ns
    mapM_ (executeOrIgnore s) groupCmds

runWithArgs :: FilePath -> [Text] -> Top -> IO ()
runWithArgs filePath args reeves = case args of
  ["-a", s] -> mapM_ (executeOrIgnore s) (topCommands reeves)
  ["-g", g, s] -> doGroup reeves g s
  [n, s] -> maybe (return ()) (executeOrIgnore s) (HashMap.lookup n $ topCommands reeves)
  [] -> do
    putStrLn $ "File: " ++ filePath
    putStrLn $ "Commands:"
    mapM_ (printCommands "  ") (List.sortBy (comparing fst) . HashMap.toList . topCommands $ reeves)
    putStrLn $ "Groups:"
    printGroups "  " (topGroups reeves)
  _ -> putStrLn "Invalid arguments: use `reeves -a <cmd>` or `reeves <name> <cmd>`"

main :: IO ()
main = do
  args <- (fmap . fmap) decodeUtf8 Byte.getArgs
  parseJson filePath (runWithArgs filePath args)
  where
  filePath = "reeves.json"
