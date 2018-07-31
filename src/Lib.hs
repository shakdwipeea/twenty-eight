module Lib
    ( someFunc
    ) where


data Suit = Club
          | Diamond
          | Heart
          | Spade
          deriving (Eq, Ord, Show, Enum)

data Rank = Jack | Queen | King | Ace | Two | Three | Four | Five
          | Six | Seven | Eight | Nine | Ten
              deriving (Eq, Ord, Show, Enum)


-- newtype NumberCard = UnsafeMkNumberCard { n :: Int }

-- mkNumberCard :: Int -> Maybe NumberCard
-- mkNumberCard n
--   | n `elem` [2 .. 10] = Just (UnsafeMkNumberCard n)
--   | otherwise = Nothing


-- data Rank = Rank FaceCard | NumberCard
--           deriving (Eq, Ord, Show)

newtype Points = UnsafeMkPoints { p :: Int }
               deriving (Eq, Ord, Show)

mkPoints :: Rank -> Points
mkPoints Jack = UnsafeMkPoints 3
mkPoints Nine = UnsafeMkPoints 2
mkPoints Ace = UnsafeMkPoints 1
mkPoints Ten = UnsafeMkPoints 1
mkPoints _ = UnsafeMkPoints 0


data Card = Card {suit :: Suit,
                  rank :: Rank,
                  points :: Points}
            deriving (Eq, Show)

type Deck = [Card]

genDeck :: Deck
genDeck = [Card suit rank (mkPoints rank) | suit <- [Club .. Spade], rank <- [Jack .. Ten]]

someFunc :: IO ()
someFunc = putStrLn "Hello there!!"
