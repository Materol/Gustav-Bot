module Commands.Morse
    (
      morseCode
    ) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

import           Control.Monad (when, void)
import Data.Map (Map)
import qualified Data.Map as Map
import           System.Random
import Data.Maybe
import           UnliftIO.Concurrent
import           Data.Char (toUpper)
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Control.Exception (asyncExceptionFromException)
import Text.ParserCombinators.ReadP (string)

morseMap = Map.fromList  [('A', ".-"),     ('B', "-..."),   ('C', "-.-."),   ('D', "-.."), -- map of character (char key) to string of morse replacement. 
                          ('E', "."),      ('F', "..-."),   ('G', "--."),    ('H', "...."),
                          ('I', ".."),     ('J', ".---"),   ('K', "-.-"),    ('L', ".-.."),
                          ('M', "--"),     ('N', "-."),     ('O', "---"),    ('P', ".--."),
                          ('Q', "--.-"),   ('R', ".-."),    ('S', "..."),    ('T', "-"),
                          ('U', "..-"),    ('V', "...-"),   ('W', ".--"),    ('X', "-..-"),
                          ('Y', "-.--"),   ('Z', "--.."),   ('0', "-----"),  ('1', ".----"),
                          ('2', "..---"),  ('3', "...--"),  ('4', "....-"),  ('5', "....."),
                          ('6', "-...."),  ('7', "--..."),  ('8', "---.."),  ('9', "----."),
                          ('.', ".-.-.-"), (',', "--..--"), ('?', "..--.."), ('\'' , ".----."),
                          ('!', "-.-.--"), ('/', "-..-."),  ('(', "-.--.-"), (')', "---.."),
                          (':', "---..."), (';', "-.-.-."), ('=', "-...-"),  ('+', ".-.-."),  
                          ('-', "-....-"), ('"', ".-..-."), ('@', ".--.-."), (' ', "/")]
-- take a snoflake of the channel to post and the string to convert
morseCode :: Snowflake -> [Char]  -> DiscordHandler ()
morseCode channelId message = 
  do
      void $ restCall $ R.CreateMessage channelId (T.pack output)
        where output = concat (getMorseString (map toUpper message)) -- standardises it to upper case, since the morse map uses caps.
-- exclusive recursion to go through every char. Using guards for style, check if it is a member of the map, if so, we can use fromJust to securely get the morse value of the character key.
getMorseString :: [Char] -> [String]
getMorseString (x:xs) -- if there is no match, we just keep the original character.
    | x `Map.member` morseMap = fromJust (Map.lookup x morseMap): "  ": getMorseString xs -- spaces are added for visual presentation, so not too crammed.
    | otherwise = [x]: "  ": getMorseString xs -- we put the char in a list to turn it into a string (since string==[char])
getMorseString _ = []







