{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant if" #-}
module Commands.HurdleChain
    (
        addGame
        ,startGames
        ,checkGameword
        ,HurdleChainGame
    ) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

import           Control.Monad (when, void)
import Data.Map (Map)
import qualified Data.Map as Map

import           System.Random
import Data.Maybe
import           Data.Char (toUpper)
import           Data.Text
import qualified Data.Text.IO as TIO
import           Data.Time.Clock
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import qualified Discord.Internal.Rest as R
import Control.Exception (asyncExceptionFromException)
import Text.ParserCombinators.ReadP (string)
import           Data.Map
import Utils.ChainWords (allowedWords)
import Control.Monad.Reader (ask, runReaderT)
-- my custom datat type for storing an instance of a game.
data HurdleChainGame = HurdleChain {
  currentWord :: Text, -- the current word to be 'beat'.
  saidWords :: [Text], -- the list of words that have been already used in the game.
  time :: UTCTime, -- the time the last valid word was said (to compare to current time)
  winningUser :: User -- the user who has the most recent winning message.
}
-- here we initialise a thread to check the games for timeouts. MVar is needed to pass it into the thread function.
startGames :: MVar (Map Snowflake HurdleChainGame) -> DiscordHandler() 
startGames hurdleMap = do
    handle <- ask -- this was something I found out by contacting the main developer of the library to see how to lower DiscordHandler -> IO() so forkIO could be used. Another recommendation I got was to UnliftIO's forkIO which is polymorphic. I believe this demonstrates understanding beter.
    threadId <- liftIO $ forkIO $ runReaderT (gameTimeThread hurdleMap) handle -- DiscordHandler() is a ReaderT monad, so it has a handle with it to be run with ReaderT. I take out the handle with ask, and then return it back with runReaderT after passing it back as an arguement. This allows me to use it as an IO for forking the thread.
    return () 

gameTimeThread :: MVar (Map Snowflake HurdleChainGame) -> DiscordHandler ()
gameTimeThread hurdleMap = forever $ do -- we run it forever, to check for games for e t e r n i t y
        void $ checkGameTime hurdleMap -- function to check any games with expired time
        void $ liftIO $ threadDelay 1000 -- a delay of 0.001 seconds between each check.

checkGameTime :: MVar (Map Snowflake HurdleChainGame) -> DiscordHandler ()
checkGameTime hurdleMap = do
    gameMap <- liftIO $ readMVar hurdleMap -- read the map of games out of the MVar
    currentTime <- liftIO getCurrentTime -- get the current time, liftIO being used here and above to use IO in DiscordHandler() type function.
    let allGames = toList gameMap
    let expiredGame = getExpiredGame allGames currentTime -- get the first expired game we can
    if isJust expiredGame -- use if statements to see if the returned maybe is a just or nothing
        then do -- here we can safely use fromJust since we did an if statement to check that it 'isJust'.
            let hurdleGame = fromJust $ Map.lookup (fromJust expiredGame) gameMap --get necessary details from the map to compose a win message
            let username = T.unpack $ userName (winningUser hurdleGame)
            let winMessage = T.pack $ getWinMessage hurdleGame username -- compose win message
            void $ restCall $ R.CreateMessage (fromJust expiredGame) winMessage -- send it
            let newMap = Map.delete (fromJust expiredGame) gameMap -- delete current key/value pair int he map
            void $ liftIO $ swapMVar hurdleMap newMap -- swap out the current map with the new one in the MVar.
    else
        return () -- then 'Nothing' was returned, so do nothing.
-- here i get the basis of the win / game over message for the game in a haskell style. Composed of the passed username, and output of function below.
getWinMessage :: HurdleChainGame -> String -> String
getWinMessage hurdleGame username = "```\ngameOver :: [Text] -> Winner()\ngameOver words =\n\tdo\n\t\ttimesUp <- True\n" ++ allWords ++ "\t\treturn $ Winner @" ++ username ++ "```"
    where allWords = getWordList (saidWords hurdleGame) 0
-- here i explicity recurse through every word said within the game, and add it in the format of a monad 'do' and a counter to show index of words said.
getWordList :: [Text] -> Int -> String
getWordList (x:xs) count = ("\t\tword") ++ (show count) ++ " <- " ++ word ++ "\n" ++ getWordList xs (count +1) 
    where word = T.unpack x 
getWordList _ count = []
-- here, explicit recursion with guards (for a stylistic choice) are used to compare the currenttime and the time stored in the game.
getExpiredGame :: [(Snowflake, HurdleChainGame)] -> UTCTime -> Maybe Snowflake
getExpiredGame ((x,y):xs) currentTime
    | diffUTCTime currentTime (time y) > 10 = Just x -- if the time difference is more than 10, it's game over. Return the game that has expired.
    | otherwise = getExpiredGame xs currentTime -- otherwise, check the next game.
getExpiredGame _ currentTime = Nothing -- if none were found (base case reached) return 'nothing'
-- here a game is added when a command is sent with a valid start word and no current games in the same channel
addGame :: MVar (Map Snowflake HurdleChainGame) -> [Char] -> Snowflake -> User -> DiscordHandler()
addGame hurdleMap word channelId user =
    do
        map <- liftIO $ readMVar hurdleMap -- get the map from my MVar
        if channelId `Map.member` map  -- check for an existing game in the map in this channe, so only 1 can run at a time in one channel.
            then do
            void $ restCall $ R.CreateMessage channelId (T.pack "`Game already going here!`")
        else if Prelude.map Data.Char.toUpper word `notElem` allowedWords -- check whether the starting word is a valid word in the list of words.
            then do 
            void $ restCall $ R.CreateMessage channelId (T.pack "`Invalid Start Word`")
        else
            do 
            currentTime <- liftIO getCurrentTime -- get current time
            let normalisedWord = Prelude.map Data.Char.toUpper word 
            let newmap = Map.insert channelId (HurdleChain {currentWord = T.pack word, saidWords = [T.pack normalisedWord], time = currentTime, winningUser = user}) map -- add our new game to the map. Technically a word has been said already, so add to saidWords.
            void $ restCall $ R.CreateMessage channelId (T.pack ("`Starting game! First word is " ++ normalisedWord ++ ". You have 10 seconds!`"))
            void $ liftIO $ swapMVar hurdleMap newmap -- update the Mvar with the modified map.
-- to update a given game with the passed arguements.
updateGame :: MVar (Map Snowflake HurdleChainGame) -> Snowflake -> [Char] -> [Char] -> User -> DiscordHandler()
updateGame hurdleMap channelId newWord previousWord user = do
    map <- liftIO $ takeMVar hurdleMap -- take the current mvar, to replcae.
    currentTime <- liftIO getCurrentTime -- get current time
    let oldPreviousWords = saidWords $ fromJust $ Map.lookup channelId map -- get the old words
    let newGame game = Just (HurdleChain {currentWord = T.pack newWord, saidWords = oldPreviousWords ++ [T.pack previousWord], time = currentTime, winningUser = user }) -- to use the update function, a function to put the key through needs to be used, which outputs a maybe.
    let newmap = update newGame channelId map  -- but we don't need to manage nothings in 'newGame' since we are CERTAIN the key will return a value to update, or the updatefunction would've never got called
    liftIO $ putMVar hurdleMap newmap -- then put the new map into the MVar.
-- here a check on the input word by a user is done to see if it is a valid answer.
checkGameword :: Message -> MVar(Map Snowflake HurdleChainGame) -> DiscordHandler ()
checkGameword message hurdleChain = do
    gameMap <- liftIO $ readMVar hurdleChain -- get a copy of the current game map
    if messageChannelId message `Map.member` gameMap -- if the channel is currently in a game, check if the last letter of the current word is the same as the first of the picked & the word is valid (in the allowed words file and is not a previously said word) and the user is not the current winner
        then do 
            let previousWord = Prelude.map Data.Char.toUpper $ T.unpack (currentWord $ fromJust $ Map.lookup (messageChannelId message) gameMap)
            let previouswords = saidWords $ fromJust $ Map.lookup (messageChannelId message) gameMap
            let user = winningUser (fromJust $ Map.lookup (messageChannelId message) gameMap)
            if Prelude.last previousWord == Prelude.head pickedWord && pickedWord `elem` allowedWords && T.pack pickedWord `notElem` previouswords && messageAuthor message /= user
                then -- if all of those are true, we add a new instance of game to the map, and update the MVar.
                do
                    void $ updateGame hurdleChain (messageChannelId message) pickedWord pickedWord (messageAuthor message)
                    goodReaction -- add a tick to confirm their message was a good answer.
                    void $ restCall $ R.CreateMessage (messageChannelId message) (T.pack $ "`Nice! New word: " ++ pickedWord ++ ". You have 10 seconds!`")
            else badReaction -- if the message was not a valid answer, but in a game channel, add a cross to signify they diddly darn messed up.
    else return ()--otherwise do nothing about the message.
            where
                pickedWord = Prelude.map Data.Char.toUpper (T.unpack $ messageContent message) -- normalise the word
                badReaction = void $ restCall $ R.CreateReaction (messageChannelId message, messageId message) (T.pack "x")
                goodReaction = void $ restCall $ R.CreateReaction (messageChannelId message, messageId message) (T.pack "white_check_mark")


 
