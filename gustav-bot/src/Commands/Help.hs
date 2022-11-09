module Commands.Help
    ( help
    ) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isInfixOf, isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Control.Exception (asyncExceptionFromException)
-- a simple help command with some instructions formatted out to be DMd.
help :: Snowflake -> DiscordHandler ()
help userId = 
    do 
        Right dmChannel <- restCall $ R.CreateDM userId -- we get the right value of the either returned by creatDM, right being the channel. There we get the channelId to use to create the message below.
        void $ restCall $ R.CreateMessage (channelId dmChannel) (T.pack "```Thank you for using Gustav Bot!\nAs you probably already realise, my prefix is '>>=' (smart I know)\
        \\nHere are my currently implemented commands:\
        \\n\tpesterAsher :: Will send a pester message to my friend Asher! (He is very happy to get pestered and completely consented!)\
        \\n\tmorse :: Will translate any string input as an arguement into morse code. Who knows when you might need a secret code?\
        \\n\thchain :: The famous (completely original idea, uninspired by previous courseworks) of the top hit game Hurdle, combined with a word chain game:\
        \\n\t\tThe game is simple. You start the game off by running the command with a 5 letter word as an arguement.\ 
        \\n\t\tFrom that point, users will type words with the following rules:\
        \\n\t\t\t1 = The word must start with the last letter of the previous word.\
        \\n\t\t\t2 = The word must be 5 letters long, and a valid english word.\
        \\n\t\t\t3 = Any valid given word can not be repeated.\
        \\n\t\t\t4 = A user can not enter a valid word twice in a row, so a different user will answer every time.\
        \\n\t\tThe game ends after 10 seconds without a valid answer, with the person who entered a valid word last winning.```")
  