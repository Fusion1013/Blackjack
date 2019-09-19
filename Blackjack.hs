module Blackjack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Example Card 1
aCard1 :: Card
aCard1 = Card (Numeric 2) Hearts

-- Example Card 2
aCard2 :: Card
aCard2 = Card Jack Hearts

aCard3 :: Card
aCard3 = Card Ace Hearts

-- Example Hand
aHand :: Hand
aHand = [aCard1, aCard2]

aHand2 :: Hand
aHand2 = [aCard3, aCard3, aCard2]

aDeck :: Deck
aDeck = [aCard3, aCard3, aCard2]

-- Prints the given hand
display :: Hand -> String
display [] = ""
display (x:xs) = (cardToString x) ++ ", " ++ (display xs)

-- Converts the given card to a formatted string
cardToString :: Card -> String
cardToString card = formatRank (rank card) ++ " of " ++ show (suit card)

-- Returns the formatted rank
formatRank :: Rank -> String
formatRank (Numeric n) = show n
formatRank rank = show rank

-- Returns the value of an entire hand
value :: Hand -> Int
value hand | rest > 10 = rest + aces
           | aces >= 2 = rest + aces
           | otherwise = rest + (aces * 11)
  where -- Saves the values to simplify the function above
    aces = numberOfAces hand
    rest = sum (map valueCard hand)

-- Returns the value of a card
valueCard :: Card -> Int
valueCard card = valueRank (rank card)

-- Returns the value of a rank
valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank rank | rank == Ace = 0 -- Aces are added in the "value" function
               | otherwise = 10

-- Counts the number of aces in a given hand
numberOfAces :: Hand -> Int
numberOfAces hand = numTimesFound Ace (map rank hand)

-- Checks the number of occurances of the input x in the list xs
numTimesFound :: Rank -> [Rank] -> Int
numTimesFound _ [] = 0
numTimesFound ace rankList = (length . filter (== ace)) rankList

-- Returns true if the game is over
gameOver :: Hand -> Bool
gameOver hand | value hand > 21 = True
              | otherwise = False

-- Returns the winner given the two hands
winner :: Hand -> Hand -> Player
winner guestHand bankHand | value guestHand <= value bankHand = Bank
                          | value guestHand > 21 = Bank
                          | otherwise = Guest

--- TASK B1 ---

-- A list containing all possible cards
fullDeck :: Deck
fullDeck = [Card x y | x <- allRanks, y <- allSuits]

-- Returns a list of all ranks
allRanks :: [Rank]
allRanks = Jack : (King : (Queen : (Ace : [(Numeric x) | x<-[2..10]])))

-- Returns a list of all suits
allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

-- Tests if the deck contains all cards
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

--- TASK B2 ---

-- Draws a card from the deck and adds it to the hand
draw :: Deck -> Hand -> (Deck,Hand)
draw [] _ = error "draw: The deck is empty"
draw (x:xs) hand = (xs, x : hand) -- ELEMENT : [SAKER] = [ELEMENT, SAKER]

--- TASK B3 ---

-- Plays the bank
playBank :: Deck -> Hand
playBank deck = pullBankCards (deck, [])

-- Pulls card for the bank until the value of the bank's hand >= 16
pullBankCards :: (Deck, Hand) -> Hand
pullBankCards (deck, bankHand) | value bankHand < 16 = pullBankCards (deck', bankHand')
                               | otherwise = bankHand
  where (deck', bankHand') = draw deck bankHand

--- TASK B4 ---
-- Shuffles the input deck using the random list given
shuffle :: [Double] -> Deck -> Deck
shuffle r deck = shuffle' r deck []

shuffle' :: [Double] -> Deck -> Deck -> Deck
shuffle' _      []   newDeck = newDeck
shuffle' (x:xs) deck newDeck = shuffle' 
         xs (removeElement (pos x deck) deck) ((getElement (pos x deck) deck) : newDeck)
pos :: Double -> Deck -> Int
pos x list = round (x * (fromIntegral(length list) - 1))

-- Returns the element at position n in a deck
getElement :: Int -> Deck -> Card
getElement n [] = error "List is empty"
getElement n list = list !! n

-- Removes an element from a deck at the give index
removeElement :: Int -> Deck -> Deck
removeElement _ [] = []
removeElement n (x:xs) | n == 0 = xs
                       | otherwise = x : removeElement (n-1) xs

--- TASK B5 ---
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
    length (shuffle randomlist deck) == length deck

--- TASK B6 ---
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
