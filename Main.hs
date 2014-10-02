{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)
import Data.List (intersperse)

import qualified Data.ByteString.Char8 as BS


data Ability = Ability {
    name :: String,
    prerequisites :: [[String]],
    level :: String,
    xp :: Int,
    description :: String}
    deriving (Show)

instance FromJSON Ability where
    parseJSON (Object v) = Ability <$>
                           v .: "Name" <*>
                           v .: "Prerequisites" <*>
                           v .: "Level" <*>
                           v .: "XP" <*>
                           v .: "Description"
    parseJSON _ = error "Can't parse ability from YAML"

quoted :: String -> String
quoted thing = "\"" ++ thing ++ "\""

style :: String -> String
style "black" = ""
style colour  = " [color=" ++ colour ++ "]"

arrows :: [Ability] -> String    
arrows abilities =
    let colours = ["black", "blue", "green", "orange"]
    in foldl (\acc ability -> acc ++ foldl (\acc2 prereqlist -> acc2 ++ foldl (\acc3 prereq -> acc3 ++ (quoted $ fst prereq) ++ " -> " ++  (quoted $ name ability) ++ (style $ snd prereq) ++ ";\n") "" (map (\foo -> (foo, snd prereqlist)) $ fst prereqlist )) "" (zip (prerequisites ability) colours)) "" abilities

prereq_to_span :: Ability -> String
prereq_to_span ability = concat $ intersperse " *or* " $ map (\conj -> concat $ intersperse ", " conj) (prerequisites ability)

ability_square :: Ability -> String
ability_square ability = concat [
              "*", name ability, "*\n",
              "XP Cost: ", show (xp ability), " (", level ability, ")\n",
              "_Prerequisites:_ ", prereq_to_span ability, "\n",
              description ability, "\n"]

ability_squares :: [Ability] -> String
ability_squares abilities = "\n[cols=\"2\"]\n|===\n" ++ (concat (map (\ability -> "|\n" ++ (ability_square ability) ++ "\n") abilities)) ++ "|\n|===\n"


main = do
         ymlData <- BS.readFile "abilities.yml"
         let abilities = fromJust (Data.Yaml.decode ymlData :: Maybe [Ability])
         frontmatter <- readFile "frontmatter.asciidoc.part"
         let start = "\n[graphviz, foo, png, width=\"500\", height=\"900\"]\n....\ndigraph G {\n"
         let end  = "}\n....\n"
         let diagram = arrows abilities
         putStrLn $ frontmatter ++ (ability_squares abilities) ++ start ++ diagram ++ end
