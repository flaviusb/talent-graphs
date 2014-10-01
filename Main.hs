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

quoted :: String -> String
quoted thing = "\"" ++ thing ++ "\""

style :: String -> String
style "black" = ""
style colour  = " [color=" ++ colour ++ "]"

main = do
         ymlData <- BS.readFile "abilities.yml"
         let colours = ["black", "blue", "green", "orange"]
         let abilities = fromJust (Data.Yaml.decode ymlData :: Maybe [Ability])
         let start = "digraph G {\n"
         let end  = "}"
         let arrows = foldl (\acc ability -> acc ++ foldl (\acc2 prereqlist -> acc2 ++ foldl (\acc3 prereq -> acc3 ++ (quoted $ fst prereq) ++ " -> " ++  (quoted $ name ability) ++ (style $ snd prereq) ++ ";\n") "" (map (\foo -> (foo, snd prereqlist)) $ fst prereqlist )) "" (zip (prerequisites ability) colours)) "" abilities
         putStrLn $ start ++ arrows ++ end
