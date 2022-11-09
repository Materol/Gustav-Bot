module Commands.PesterAsher
    ( pesterAsh
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

pesterAsh :: Snowflake -> DiscordHandler (Either RestCallErrorCode Message)
pesterAsh channel = 
    do  -- We get the right value of the either returned by 'CreateDM'
        Right dmChannel <- restCall $ R.CreateDM (read "287967880711110656") -- just to confirm, he has fully consented to this and enjoys getting pestered.
        restCall $ R.CreateMessage (channelId dmChannel) (T.pack "`GET PESTERED.`") -- send message to the channelId (the created DM)
        restCall $ R.CreateMessage channel (T.pack "`Succesfully pestered.`")