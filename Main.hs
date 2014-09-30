{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS


data Ability = Ability {
    name :: String,
    prerequisites :: [[String]]}
    deriving (Show)

instance FromJSON Ability where
    parseJSON (Object v) = Ability <$>
                           v .: "Name" <*>
                           v .: "Prerequisites"
    parseJSON _ = error "Can't parse ability from YAML"

main = do
         ymlData <- BS.readFile "abilities.yml"
         let abilities = Data.Yaml.decode ymlData :: Maybe [Ability]
         -- Print it, just for show
         print $ fromJust abilities
