{-# LANGUAGE TemplateHaskell #-}
-- using template haskell, we can embed our file into the code during compilation and access it whenever.
module Utils.ChainWords where


import Data.FileEmbed ( embedStringFile )
import Data.List (sort)
-- allowed words borrowed from Hurdle. (combination of both the answers and allowedguesses in hurdle, sorted alphabetically.)
allowedWords :: [String]
allowedWords =  lines $(embedStringFile "assets/chainablewords")