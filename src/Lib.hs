{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( someFunc
    ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent.STM.TChan

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
                     ,writeChan :: TChan PlayerMessage
                     ,readChan :: TChan DealerMessage}
            deriving (Eq, Show)

instance Show (TChan c)  where
  show c = "A channel " ++ show c

data Game = Game {players :: [Player]
                 ,deck :: Deck}
          deriving (Eq, Show)

type Name = String

mkPlayer :: Name -> IO Player
mkPlayer n = do
  writeChan <- atomically $ newTChan
  readChan <- atomically $ newTChan
  return Player {name = n
                ,score = 0
                ,hand = []
                ,writeChan = writeChan
                ,readChan = readChan}

genTestPlayers :: IO [Player]
genTestPlayers = mapM (\i -> mkPlayer $ "player-" ++ show i) [1 .. 4]

mkGame :: IO Game
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


-- startGame :: Game
-- startGame = let game = dealCards mkGame in
  
someFunc :: IO ()
someFunc = putStrLn "Hello there!!"
