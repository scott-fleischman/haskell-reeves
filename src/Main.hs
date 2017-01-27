module Main where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Config
  = Config
  { items :: HashMap Text Item
  }

data Item
  = ServiceItem Service
  | ReferenceItem Text

data Service
  = Service
  { beforeStart :: [Command]
  , start :: [Command]
  , stop :: [Command]
  }

data Command
  = ShellCommand Text
  | DockerCommand Docker

data Docker = Docker

main :: IO ()
main = do
  putStrLn "turtle"
