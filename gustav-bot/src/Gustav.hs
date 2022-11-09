{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module Gustav
    ( mainStuff
    , eventHandler
    , HurdleChainGame
    ) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import           Commands.Help
import           Commands.PesterAsher
import           Commands.Morse
import           Commands.HurdleChain
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isInfixOf, isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO
import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Control.Exception (asyncExceptionFromException)
import qualified Data.IntMap as Map
import Data.Map
import Control.Monad.Cont
import Control.Monad.RWS
import Control.Monad.Reader

prefix = T.pack ">>=" -- prefix for the bot.

mainStuff :: IO ()
mainStuff = do
    token <- TIO.readFile "./src/token.txt" -- read in the token externally for safety, in case the code is ever leaked or revealed by accident, the bot is safe unless system files are compromised.
    hurdleMap <- newMVar $ fromList [] -- create a new MVar for storing the map for the hurdleChain game.
    error <- runDiscord $ def -- curried functions to start, runs DiscordHandler() and outputs an error if it happens into console when it gets it.
             { discordToken = T.pack (T.unpack token)
             , discordOnEvent = eventHandler hurdleMap -- runs when an event happens (such as a message being sent)
             , discordOnStart = startGames hurdleMap -- runs on start of discord, and runDiscord allows to run DiscordHandler() types in main. Starts the thread for the hurdleMap games.
             } 
    TIO.putStrLn error -- converts whatever output error from 'runDiscord' into IO from Text since mainStuff is an IO() type.
-- this function is run everytime an event the bot sees happens. 
eventHandler :: MVar (Map Snowflake HurdleChainGame) -> Event -> DiscordHandler ()
eventHandler hurdleMap event = case event of
    MessageCreate m -> -- only implementation the bot has is for messages.
        if not (fromBot m) && hasOomfie m -- checks if the message is from the bot, and if not, whether it contains the bad word.
        then do
            void $ restCall $ R.CreateReaction (messageChannelId m, messageId m) (T.pack "hammer") -- if so, we promplty react to the message and give feedback to the user.
            void $ restCall $ R.CreateMessage (messageChannelId m) (T.pack "Banned")
            void $ banUser (messageGuildId m) (userId (messageAuthor m)) -- and banning them for their bad behaviour.
        -- the use of the word above is not allowed under any circumstances, so only after checking for it, we can check for commands.
        else if not (fromBot m) && isCommand m -- if not from a bot, and the type of message is a command.
            then if getCommand m == "help" -- go through every comparison of all possible commands.
                then do
                    void $ help (userId (messageAuthor m))
            else if getCommand m == "pesterash"
                then do
                    void $ pesterAsh (messageChannelId m)
            else if getCommand m == "morse"
                then do
                    void $ morseCode (messageChannelId m) (getCommandContent m)
            else if getCommand m == "hchain"
                then do -- here we pass a few more arguements than usual, most notably 'hurdleMap', the MVar storing the dictionary for games. Very important.
                    void $ addGame hurdleMap (getCommandContent m) (messageChannelId m) (messageAuthor m)
            else -- if there's no matches, command not found.
                void $ restCall $ R.CreateMessage (messageChannelId m) (T.pack "Command Not Found.")
        else if not (fromBot m) -- finally, if it's not a command, then do things such as checking for current games to compare the message to.
            then void $ checkGameword m hurdleMap
        else
            return () -- otherwise, just an average bot message.
    _ -> return () -- to note, i could've nested everything in 'not (fromBot m)' but I prefer it being more inline without an extra indentation like that.
-- just check whether message is from a bot using a premade discord-haskell function.
fromBot :: Message -> Bool
fromBot message = userIsBot (messageAuthor message)
-- check whether the message is a command by using 'isPrefixOf' with the bot prefix on the message.
isCommand :: Message -> Bool
isCommand message = prefix `isPrefixOf` toLower (messageContent message)
-- extract the command from the user input
getCommand :: Message -> [Char]
getCommand message = takeWhile (/= ' ') $ Prelude.drop (length (T.unpack prefix)) text -- by clearing all the leading whitespaces after the command, by dropping the first x characters (x being the prefix, ours is >>=) and then remove leading whitespaces using takewhile.
    where
        text = T.unpack (toLower (messageContent message))
-- get the text after the command 
getCommandContent :: Message -> [Char] 
getCommandContent message 
    | not (Prelude.null (dropWhile (/= ' ') text)) = tail $ dropWhile (/= ' ') text -- here we drop all characters until we reach a whitespace (which we remove with tail) which signifies a spaces after a command, hence the commant arguement.
    | otherwise = "" -- if it's empty, after 'dropWhile', we don't want to get tail since we might get a crash.
    where
        text = T.unpack (messageContent message)
-- check for the forbidden word using 'isInfixOf'.
hasOomfie :: Message -> Bool
hasOomfie message = (T.pack "oomfie") `isInfixOf` toLower (messageContent message)
-- function to ban the given user id in the given maybe guildId. Give them a reason for the ban.
banUser :: Maybe Snowflake -> Snowflake -> DiscordHandler (Either RestCallErrorCode ())
banUser (Just guildId) userId = restCall $ R.CreateGuildBan guildId userId (R.CreateGuildBanOpts (Just 0) (Just (T.pack "No Ooomfing Allowed")))
banUser Nothing userId = error "bad cuz mad" -- incase nothingis passed instead of a just (should not happen.)


