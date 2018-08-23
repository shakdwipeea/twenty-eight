{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM.TChan
import Control.Concurrent
import System.Random.Shuffle
import Control.Monad.Random.Lazy

import qualified Network.WebSockets as WS

data Suit = Club
          | Diamond
          | Heart
          | Spade
          deriving (Eq, Ord, Show, Enum)

data Rank = Jack | Nine | Ace | Ten | King | Queen  | Eight | Seven
          | Two | Three | Four | Five | Six
          deriving (Eq, Ord, Show, Enum)

newtype Points = Points { p :: Int }
               deriving (Eq, Ord, Show)

mkPoints :: Rank -> Points
mkPoints Jack = Points 3
mkPoints Nine = Points 2
mkPoints Ace = Points 1
mkPoints Ten = Points 1
mkPoints _ = Points 0

data Card = Card {suit :: Suit,
                  rank :: Rank,
                  points :: Points}
            deriving (Eq, Show)

type Deck = [Card]

genDeck :: Deck
genDeck = [Card suit rank (mkPoints rank) | suit <- [Club .. Spade], rank <- [Jack .. Ten]]

isPlayableRank :: Rank -> Bool
isPlayableRank rank
  | rank `elem` [Jack , Nine , Ace , Ten , King , Queen  , Eight , Seven] = True
  | otherwise = False

getPlayableCards :: Deck
getPlayableCards = filter (isPlayableRank . rank)  genDeck


data RedealReply = Redeal | Continue
                 deriving (Eq, Show)

data PlayerMessage = MRedeal RedealReply
             deriving (Eq, Show)

data RedealMsg = WantRedeal deriving (Eq, Show)

data DealerMessage = MRedealQ RedealMsg
                     deriving (Eq, Show)

data Player = Player {name :: String
                     ,score :: Int
                     ,hand :: Deck
                     ,pChan :: TChan PlayerMessage
                     ,dChan :: TChan DealerMessage}
            deriving (Eq, Show)

instance Show (TChan c)  where
  show c = ""


data Game = Game {players :: [Player]
                 ,deck :: Deck}
          deriving (Eq, Show)

type Name = String

mkPlayer :: Name -> STM Player
mkPlayer n = do
  writeChan <- newTChan
  readChan <- newTChan
  return Player {name = n
                ,score = 0
                ,hand = []
                ,pChan = writeChan
                ,dChan = readChan}

genTestPlayers :: STM [Player]
genTestPlayers = mapM (\i -> mkPlayer $ "player-" ++ show i) [1 .. 4]

mkGame :: STM Game
mkGame = fmap (\p -> Game {players = p, deck = getPlayableCards})  genTestPlayers

distribute :: Player -> Card -> Player
distribute p@(Player {hand}) c = p { hand = c : hand}

dealCards :: Game -> Game
dealCards (Game {players, deck}) = let (currentDeal, restDeck) = splitAt (length players) deck
                                 in  Game { players = zipWith distribute players currentDeal
                                          , deck = restDeck}


isRedealPossible :: Game -> Bool
isRedealPossible (Game {players, deck}) = any hasPoints $ (hand . head) players
  where
    hasPoints (Card {points}) = (p points) > 0

type GameState  = StateT Game STM

playerLoop :: Player -> STM ()
playerLoop (Player {pChan, dChan}) = do
  dMsg <- readTChan dChan
  case dMsg of
    MRedealQ WantRedeal -> writeTChan pChan (MRedeal Redeal)

askForRedeal :: GameState ()
askForRedeal = do
  game <- get
  let fPlayer =  head $ players game 
  lift $ writeTChan (dChan fPlayer) (MRedealQ WantRedeal)
  lift $ playerLoop fPlayer
  playerAnswer <- lift $ readTChan (pChan fPlayer)
  return ()


startGame :: GameState ()
startGame = do
  modify dealCards
  g <- get
  when (isRedealPossible g) askForRedeal


shuffleDeck :: Game -> IO Game
shuffleDeck g@(Game {deck}) = evalRandIO $ do
  d' <- shuffleM deck
  return g {deck =  d'}

gameSequence :: GameState ()
gameSequence =
  startGame >> askForRedeal >> return ()

-- STM Game

-- StateT Game STM

socketApi :: WS.ServerApp
socketApi pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

runWebSockets :: IO ()
runWebSockets = WS.runServer "0.0.0.0" 9200 socketApi 

main :: IO ()
main = atomically mkGame >>= shuffleDeck >>= runGame >>= (putStrLn . show)
  where
    runGame :: Game -> IO Game
    runGame g = do
      (_, g') <- atomically $ runStateT gameSequence g
      return g'
  
