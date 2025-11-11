module HaskellGradingAssignment where

    -- Imports
    import System.Random 
    import Data.List
    import Data.Ord
    import Data.Maybe
    
    {- 
        Data Type Defenitions
        Defines the data types that will be used throughout the program
    -}

    data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Enum, Bounded)

    data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
        |Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum, Bounded)

    type Kicker = Rank
    
    data Card = Card { 
        rank :: Rank, 
        suit :: Suit 
        } deriving (Show, Eq)

    type Deck = [Card]

    data HandRank = HighCard Rank [Kicker]| Pair Rank [Kicker] | TwoPair Rank Rank [Kicker] 
        | ThreeKind Rank [Kicker] | Straight Rank | Flush Rank | FullHouse Rank Rank 
        | FourKind Rank [Kicker] | StraightFlush Rank| RoyalFlush deriving(Show, Eq, Ord)

    data PlayerAction = Check | Bet | Call | Raise | Fold deriving (Show, Eq)

    data PlayerRole = Random | Passive | Aggressive | Smart deriving (Show, Eq)

    data Player = Player { 
        name :: String, 
        hand :: [Card], 
        chips :: Int, 
        isDealer :: Bool, 
        behaviour :: PlayerRole,
        isActive :: Bool
    } deriving (Show, Eq)

    type Players = [Player]

    data GameState = GameState { 
        players :: Players,
        deck :: Deck,
        communityCards :: [Card],
        pot :: Int,
        bets :: [(Player, Int)],
        dealerPosition :: Int,
        smallBlindPosition :: Int,
        bigBlindPosition :: Int 
    }deriving (Show)




    -- Deck Functions

    {-
        Uses the Suit and Rank data types to create the 52 card deck
        Defines the ranges for which rank and suit will go upto
    -}
    generateDeck :: Deck
    generateDeck = [Card rank suit | rank <- [Two .. Ace], suit <- [Hearts .. Spades]]

    {-
        Generates a shuffled deck that will be used throughout the rest of the program

        The deck (list of cards) will pick a random number from 0 - 51 and choose the card
        at that position and recursively repeat this until the original deck list has been
        exhausted
    -}
    shuffleDeck :: StdGen -> Deck -> (Deck, StdGen)
    shuffleDeck gen [] = ([], gen)
    shuffleDeck gen deck = 
        let (n, newGen) = randomR (0, length deck - 1) gen
            -- Picks random card
            chosen = deck !! n 
            -- Removes chosen card from the deck
            remaining = take n deck ++ drop (n + 1) deck
            -- Recursively builds deck
            shuffledDeck = chosen : fst(shuffleDeck newGen remaining)
        in  (shuffledDeck, newGen)





    -- Deal Card Functions

    {-
        Will either deal the hole(player) cards or will deal the community cards
        where n will be the cards to deal

        Hole cards are only dealt if the Int given is 2 (cards to deal) as community
        cards will only ever deal 3 or 1 cards
    -}
    dealCards :: Int -> GameState -> GameState
    dealCards n gameState
        | n == 2 =
            let gameDeck = deck gameState
                -- Gets the updated players and updated deck
                (updatedPlayers, updatedHoleDeck) = dealCardsToPlayers (players gameState) gameDeck
                -- Updates Game State
                updatedHoldGameState = gameState{players = updatedPlayers,
                deck = updatedHoleDeck}
            in updatedHoldGameState
        | otherwise =
            let gameDeck = deck gameState
                -- Splits deck to get the com cards and updated deck
                (updatedComCards, updatedDeck) = splitAt n gameDeck
                -- Updates GameState
                updatedComGameState = gameState {communityCards = communityCards gameState 
                ++ updatedComCards, deck = updatedDeck}
            in updatedComGameState

    {-
        If hole cards need to be dealt all players will have 2 cards
        added to their hand, recursively going through the players in the game
    -}
    dealCardsToPlayers :: [Player] -> Deck -> ([Player], Deck)
    dealCardsToPlayers [] deck = ([], deck)
    dealCardsToPlayers (p:ps) deck = 
        let (handCards, updatedDeck) = splitAt 2 deck
            -- Updates players hand
            updatedPlayer = p {hand = handCards}
            (updatedPlayers, finalDeck) = dealCardsToPlayers ps updatedDeck
        in (updatedPlayer : updatedPlayers, finalDeck)

        
    


    -- Evaluate Hand Functions
    
    {-
        Inputted 5 cards
        Checks what conditions these 5 cards meets which then indicates the
        hand rank of those cards

        hasKind - Uses the grouped cards and checks if any rank appears the
        amount of times needed to fulfil a 4 of a kind, 3 of a kind or pair
        (used as a 2 of a kind in this system as they are identical)

        Each hand rank is accompanied a kicker if necessary
        For hand ranks that do not need to be accompanied by kickers it
        will be accompanied by the high card from the 5 cards
        (with the exception of a royal flush)
    -}
    evaluateHand :: [Card] -> HandRank
    evaluateHand cards
        | isFlush cards && isStraight cards && highestCard cards == Ace = RoyalFlush
        | isFlush cards && isStraight cards = StraightFlush bestCard
        | hasKind 4 = FourKind (kindRank 4) kickers
        | hasKind 3 && hasKind 2 = FullHouse (kindRank 3) (kindRank 2)
        | isFlush cards = Flush bestCard
        | isStraight cards = Straight bestCard
        | hasKind 3 = ThreeKind (kindRank 3) kickers
        | twoPair groupedCards = TwoPair (kindRank 2) secondPairRank kickers
        | hasKind 2 = Pair (kindRank 2) kickers
        -- The kickers of a high card hand will just be the rest of the cards
        | otherwise = HighCard bestCard (take 4 (map rank (sortCards cards)))
        where
            bestCard = highestCard cards
            groupedCards = groupRanks cards
            -- Checks if any of the grouped cards appear n times
            hasKind n = any ((== n) . snd) groupedCards 
            -- Gets the rank that appears n times in the grouped cards
            kindRank n = fst( head(filter ((== n) . snd) groupedCards))
            secondPairRank = fst (last (filter ((== 2) . snd) groupedCards))
            -- Gets the rest of the cards that only appear once
            kickers = [fst n | n <- groupedCards, snd n == 1]

    {-
        Generates a list of the combinations of the given cards for
        commbinations of size n

        Gets all n combinations that include c from the n-1 combinations
        of the remaining cards (cs)
        Gets all the combinations of n from the remaining cards
        (does not include c)

        These are then combined to create the list of all the n combinations
        of the cards
    -}
    combinations :: Int -> [Card] -> [[Card]]
    combinations 0 _ = [[]]
    combinations _ [] = []
    combinations n (c:cs) = [c : rest | rest <- combinations (n-1) cs] ++ combinations n cs

    {-
        Each player will have 7 total cards they can choose to create
        they're strongest 5 card hand (5 community cards + 2 hole cards).

        This gets all the different 5 card combinations of the 7 cards and
        evaluates each card combination.

        This will result in a list of different hand ranks in which the highest
        value hand rank in the list is chosen
    -}
    evaluateCombinationHands :: [Card] -> HandRank
    evaluateCombinationHands cards =
        let combinationHands = combinations 5 cards
            -- Gets list of ranks from the list of card combos
            rankCombinations = map evaluateHand combinationHands
            maxRank =  maximum rankCombinations
        in maxRank

    {-
        Will get a list of the strongest hand ranks each player can make
        The strongest rank from these will then be the 'winners' hand rank
        It will then get a list of players with this hand rank
    -}
    determineWinner :: GameState -> [Player]
    determineWinner gameState =
        -- Combines the players hand and the community cards
        let playersHands = [(player, evaluateCombinationHands (hand player ++ communityCards gameState)) |
             player <- players gameState, isActive player]
            winningRank = maximum(map snd playersHands)
            -- Gets the players with the winning hand
            winningPlayers = [player | (player, handRank) <- playersHands, handRank == winningRank]
        in winningPlayers

    {-
        Sorts the given cards by their rank
        Ascending order
    -}
    sortCards :: [Card] -> [Card]
    sortCards = sortBy (comparing rank)

    {-
        Gets the highest card rank from given cards
    -}
    highestCard :: [Card] -> Rank
    highestCard cards = maximum(map rank (sortCards cards))

    {-
        Goes through each card in the given card and checks if they
        are all the same rank
    -}
    isFlush :: [Card] -> Bool
    isFlush cards = and [suit c == suit(head cards) | c <- cards]

    {-
        Checks if the list of ranks in the incremented range from the min to the max
        in the given cards is equal to the cards sorted
    -}
    isStraight :: [Card] -> Bool
    isStraight cards = 
        let sortedRanks = map rank (sortCards cards) --Extract just the sorted ranks
        in [head sortedRanks .. last sortedRanks] == sortedRanks
 
    {-
        Gets a list of the ranks from the sorted cards and groups them together in
        seperate lists of just that rank
        The list of tuples is then created by having the rank and how many are in the
        group, resulting in a tuple of each rank and how many times it appears in the
        given cards
    -}
    groupRanks :: [Card] -> [(Rank, Int)]
    groupRanks cards = [(head g, length g) | g <- group (map rank (sortCards cards))]

    {-
        Filters the grouped cards only for ranks that appear twice (only pairs)
        Checks if there are two different ranks that appear twice
        which then indicates a two pair
    -}
    twoPair :: [(Rank, Int)] -> Bool
    twoPair gs = length (filter ((== 2) . snd) gs) == 2

    
    




    -- Betting Round Functions


    {-
        Starts the betting round cycles choosing the start position
        depending on if its a pre flop round or normal round

        Will continue cycling through betting round cycles until the betting
        round has ended correctly
    -}
    bettingRound :: GameState -> Bool -> Int -> StdGen -> IO (GameState, StdGen)
    bettingRound gameState isPreFlop roundNum gen = do
        -- Chooses the correct starting index for the betting round cycle
        let startIndex 
                | isPreFlop = (bigBlindPosition gameState + 1) `mod` length (players gameState)
                | otherwise = (dealerPosition gameState + 1) `mod` length (players gameState) 
        -- Changes the player order so it is in the correct order depending on the start index
        let playerOrder = drop startIndex (players gameState) ++ take startIndex (players gameState)
        
        (updatedGameState, newGen) <- bettingRoundCycle playerOrder isPreFlop  gameState roundNum gen

        -- Checks if the betting round has ended correctly
        if isBettingRoundOver updatedGameState
            then return (updatedGameState, newGen)
            -- Starts another cycle
            else bettingRound updatedGameState isPreFlop (roundNum+1) newGen 

    {-
        This will allow each player to have their betting turn
        For the first round (round 0) it will always have to atleast cycle through
        all the players
        For the other rounds the betting round could end at any point if the correct
        conditions are met

        For each active player they will be able to choose their action
        (depending on their behaviour) and the states will update accordingly

        Additional information for each player is also displayed for each player turn

    -}
    bettingRoundCycle :: [Player] -> Bool -> GameState -> Int -> StdGen -> IO (GameState, StdGen)
    bettingRoundCycle [] isPreFlop gameState roundNum  gen = return (gameState, gen)
    bettingRoundCycle (p:ps) isPreFlop gameState roundNum  gen
        -- Cycles through all the players for the first round
        | roundNum /= 0 && isBettingRoundOver gameState = return(gameState, gen)
        | not(isActive p) = bettingRoundCycle ps isPreFlop gameState roundNum gen
        | length (activePlayersList gameState) <= 1 = return(gameState, gen)
        | otherwise = do
            -- Get the players action for the round
            let (action, newGen) = determinePlayerAction p isPreFlop gameState roundNum gen

            -- Update the game state depending on the players action
            let (updatedGameState, newGen2) = updateGameState gameState p action newGen

            let updatedPlayer = getPlayerFromGameState (players updatedGameState) p 

            -- Displays additional information for the turn
            additionalInfoPlayerAction p updatedPlayer action updatedGameState

            -- Continues with the betting round cycle for the other players
            bettingRoundCycle ps isPreFlop updatedGameState roundNum newGen2
    

    {-
        Depending on the players set behaviour it will choose their
        player action for all the valid actions the player can do at that point

        As smart player uses its max 5 card hand to judge what action to do
        for the preflop its actions are just set to the random players actions
        (as the player will only have the 2 hole cards and no community cards)
    -}
    determinePlayerAction :: Player -> Bool -> GameState -> Int -> StdGen -> (PlayerAction, StdGen)
    determinePlayerAction p isPreFlop gameState roundNum gen 
        | behaviour p == Random = randomPlayerAction validActions gen
        | behaviour p == Passive = passivePlayerAction validActions gen
        | behaviour p == Aggressive = aggressivePlayerAction validActions gen
        | behaviour p == Smart && not isPreFlop = smartPlayerAction validActions p gameState gen
        | otherwise = aggressivePlayerAction validActions gen
            where
                -- Gets the players valid actions
                validActions = validActionList p
                       (playerBetSum p (bets gameState))
                       (raiseAmount (bets gameState))
                       (totalBetSum (bets gameState))

    {-
        Returns the player from the game state
    -}
    getPlayerFromGameState :: [Player] -> Player -> Player
    getPlayerFromGameState [] player = player
    getPlayerFromGameState (p:ps) player
        | name p == name player = p
        | otherwise = getPlayerFromGameState ps player


    {-
        Depending on the players action it will calculate the needed values
        and make updates to the player, bets and game state where needed

        Raise/bet amounts are randomly chosen in the range of the min raise
        (or 1 for min bet) and the chips of the player

        Players who fold are removed from bets and set as not active 
    -}
    updateGameState :: GameState -> Player -> PlayerAction -> StdGen -> (GameState, StdGen)
    updateGameState gameState player action gen
        | action == Check = 
            -- Empty Bet made for players who check
            let updatedBets = updateBetList (bets gameState) player 0
                updatedGameState = gameState {bets = updatedBets}
            in (updatedGameState, gen)
        | action == Call =
            -- Calculates call amount and updates the players, bets and game state
            let callAmount = totalBetSum (bets gameState) - playerBetSum player (bets gameState)
                updatedPlayer = player {chips = chips player - callAmount}
                updatedPlayerList = updatePlayerList (players gameState) updatedPlayer

                updatedPot = pot gameState + callAmount

                updatedBets = updateBetList (bets gameState) updatedPlayer callAmount

                updatedGameState = gameState {players = updatedPlayerList,
                pot = updatedPot, bets = updatedBets}
            in (updatedGameState, gen)
        | action == Bet =
            -- Randomly generates the bet amount and updates the players, bets and game state
            let (betAmount, newGen) = randomR(1 :: Int, chips player :: Int) gen
                updatedPlayer = player {chips = chips player - betAmount}
                updatedPlayerList = updatePlayerList (players gameState) updatedPlayer

                updatedPot = pot gameState + betAmount

                updatedBets = updateBetList (bets gameState) updatedPlayer betAmount

                updatedGameState = gameState {players = updatedPlayerList,
                pot = updatedPot, bets = updatedBets}
            in (updatedGameState, newGen)
        | action == Raise =
            -- Randomly generates the raise amount and updates the players, bets and game state
            -- Making sure the raise is not below the min raise amount
            let (raiseRAmount, newGen) = randomR (totalBetSum (bets gameState) + raiseAmount (bets gameState) 
                    - playerBetSum player (bets gameState) :: Int, chips player :: Int) gen
                updatedPlayer = player {chips = chips player - raiseRAmount}
                updatedPlayerList = updatePlayerList (players gameState) updatedPlayer

                updatedPot = pot gameState + raiseRAmount

                updatedBets = updateBetList (bets gameState) updatedPlayer raiseRAmount

                updatedGameState = gameState {players = updatedPlayerList,
                                          pot = updatedPot, bets = updatedBets}
            in (updatedGameState, newGen)
        | action == Fold =
            let updatedPlayer = player {isActive = False}
                updatedPlayerList = updatePlayerList (players gameState) updatedPlayer
                -- Players removed from the bets list
                updatedBets = [(p,b)  | (p,b) <- bets gameState, name p /= name updatedPlayer] 
            in (gameState {players = updatedPlayerList, bets = updatedBets}, gen)
    
    {-
        Updates the player in the player list which matches
        the given player
    -}
    updatePlayerList :: [Player] -> Player -> [Player]
    updatePlayerList [] _ = []
    updatePlayerList (p:ps) updatedPlayer
        | name p == name updatedPlayer = updatedPlayer : ps
        | otherwise = p : updatePlayerList ps updatedPlayer

    {-
        Updates the bet for the given player in the list of bets
    -}
    updateBetList :: [(Player, Int)] -> Player -> Int -> [(Player, Int)]
    -- Adds player to bet list if its their first bet
    updateBetList [] p bet = [(p, bet)] 
    updateBetList ((p, b):ps) player updatedBet
        | name p == name player = (updatedPlayer, b + updatedBet) : ps
        | otherwise = (p, b) : updateBetList ps player updatedBet
            where
                updatedPlayer = p{chips = chips p - updatedBet}

    {-
        Uses the players chips amount, raise amount, call amount and
        max current bet to calculate what actions are valid for the player
    -}
    validActionList :: Player -> Int -> Int -> Int -> [PlayerAction]
    validActionList player playerCurrentBet raiseAmount totalCurrentBet
        | totalCurrentBet == 0 && chips player > 0 = [Fold, Check, Bet]
        | totalCurrentBet == 0 = [Fold, Check]
        | chips player < (totalCurrentBet - playerCurrentBet) = [Fold]
        | chips player >= ((totalCurrentBet + raiseAmount) - playerCurrentBet) = [Fold, Call, Raise]
        | chips player >= (totalCurrentBet - playerCurrentBet) = [Fold, Call]
        | otherwise = []
    
    {-
        Calculates how many chips the player has already put in for
        the given round
    -}
    playerBetSum :: Player -> [(Player, Int)] -> Int
    playerBetSum player bets
        | null bets = 0 
        | otherwise = sum [amount | (p, amount) <- bets, name p == name player]

    {-
        Calculates the maximum current bet out of all the bets
    -}
    totalBetSum :: [(Player, Int)] -> Int
    totalBetSum bets
        | null bets = 0 
        | otherwise = maximum [score | (p, score) <- bets]

    {-
        Works out the difference between the highest bet amount and
        the second highest bet amount, indicating the minimum raise amount
    -}
    raiseAmount :: [(Player, Int)] -> Int
    raiseAmount bets
        | null bets = 0
        | otherwise =
            let betAmounts = map snd bets  
                maxBet = maximum betAmounts  
                secondMaxBet 
                    -- Second highest bet set to zero if not other bets
                    | not (any (< maxBet) betAmounts) = 0
                    | otherwise = maximum (filter (< maxBet) betAmounts)
            in maxBet - secondMaxBet  


    {-
        If conditions are met the betting round will end

        The round will end if all players have matched the bet or only one player
        remains while the number of active players being equal to the number of bets
        in the bet list

        Or if only one player remains and the bet list is empty
    -}
    isBettingRoundOver :: GameState -> Bool
    isBettingRoundOver gameState = 
            let betAmounts = map snd (bets gameState)
                activePlayers = activePlayersList gameState
                maxBet = totalBetSum (bets gameState)

            -- Round ends if all players have matched the highest bet, or only one player remains
            in (length activePlayers <= 1 || all (== maxBet) betAmounts) 
                && length activePlayers == length betAmounts ||
                (length activePlayers <= 1 && betAmounts == [])

    {-
        Gets active players from game state
    -}
    activePlayersList :: GameState -> [Player]
    activePlayersList gameState = filter isActive (players gameState)






    -- Game Round Functions


    {-
        Loops game rounds until only one player is remaining or if the number of
        game rounds reaches 100
    -}
    gameLoop :: GameState -> StdGen -> Int -> IO (GameState, StdGen)
    gameLoop gameState gen roundCount
        -- Stops game loop if conditions met
        | length (players gameState) == 1 || roundCount >= 100 = return ((endGame gameState), gen)
        | otherwise = do
            (updatedGameState, newGen) <- gameRound gameState gen
            gameLoop updatedGameState newGen (roundCount + 1)
    
    {-
        Performs the pre-setup of the game

        Goes through each betting round from pre-flop to post-river

        The winner is determined from the players left by seeing who has
        the best hand 

        Information of the winner is shown and the post-setup of the game
        is performed
    -}
    gameRound :: GameState -> StdGen -> IO (GameState, StdGen)
    gameRound gameState gen = do
        putStrLn "*************************************************"
        putStrLn ""
        putStrLn "New Game"
        putStrLn ""
        putStrLn "Setting Up Game"

        -- Shuffles the deck and deals the hole cards to the players
        let (shuffledDeck, newGen) = shuffleDeck gen (deck gameState)
        let gameStateShuffledCards = gameState{deck = shuffledDeck}
        let gameStateWithCards = dealCards 2 gameStateShuffledCards

        -- Sets the dealer and deals the blinds
        let dealerGameState = setDealer gameStateWithCards
        let blindGameState = dealBlinds dealerGameState
        
        putStrLn $ "Dealer: " ++ show (name(players gameState !! dealerPosition gameState))
        putStrLn $ "Small Blind: " ++ show (name(players gameState !! smallBlindPosition gameState))
        putStrLn $ "BigBlind: " ++ show (name(players gameState !!bigBlindPosition gameState))


        -- Performs the betting round for pre-flop
        (postPreFlopGameState, newGen2) <- bettingRound blindGameState True 0 newGen
        putStrLn ""
        putStrLn  "End of Pre-flop"

        -- Deals the 3 community cards
        let gameStateAfterFlop = dealCards 3 (resetBets postPreFlopGameState)
        
        (postFlopGameState, newGen3) <- bettingRound gameStateAfterFlop False 0 newGen2
        putStrLn "" 
        putStrLn "End of Pre-Turn"

        -- Performs the betting round for pre-turn
        let gameStateAfterTurn = dealCards 1 (resetBets postFlopGameState)
       
        (postTurnGameState, newGen4) <- bettingRound gameStateAfterTurn False 0 newGen3
        putStrLn ""
        putStrLn "End of Pre-River"

        -- Deals a community card
        let gameStateAfterRiver = dealCards 1 (resetBets postTurnGameState)

        -- Performs the betting round for pre-river
        (postRiverGameState, newGen5) <- bettingRound gameStateAfterRiver False 0 newGen4
        putStrLn ""
        putStrLn "End of Pre-Showdown"

        -- Prints additional information about the players hands who are in the showdown
        printPlayerHands postRiverGameState
        -- Evaluate the hands of the players and determines the winners
        let winners = determineWinner postRiverGameState
        putStrLn $ "Game Round Winners: " ++ showPlayers winners
        putStrLn "Distributing Pot to Winners"
        putStrLn ""
        putStrLn "*************************************************"

        -- Distributes the pot equally to the winners
        let postWinnersGameState = distributeWinnings winners postRiverGameState
        let nextDealerPos = findNextDealerPosition (players postWinnersGameState) 
                (players (removeBustPlayers postWinnersGameState)) (dealerPosition postWinnersGameState)

        -- Performs the post game round operations, so the game state is ready for another round
        let updatedGameState = rotatePositions(clearComCards (resetActivePlayers
             (removeBustPlayers (resetBets postWinnersGameState)))) nextDealerPos
        let newDeck = generateDeck

        let finalGameState = resetDealer updatedGameState{deck = newDeck}
        putStrLn ""
        return (finalGameState, newGen5)
    
    {-
        Gets the winner from the players by who has the most chips
    -}
    endGame :: GameState -> GameState
    endGame gameState =
        let finalPlayers = players gameState
            maxChips = maximum (map chips finalPlayers)
            winners = [player | player <- finalPlayers, chips player == maxChips]
        in gameState {players = winners}


    {-
        Gets the player who are in the small and big blind position
        Calculates the small and big blinds new chips amount and adds the blinds to the pot
    -}
    dealBlinds :: GameState -> GameState
    dealBlinds gameState =
        let smallBlindPlayer = (players gameState !! smallBlindPosition gameState)
                {chips = chips (players gameState !! smallBlindPosition gameState) - 5}
            smallBlindGameState = gameState{players = updatePlayerList (players gameState) smallBlindPlayer,
            pot = pot gameState + 5, bets = bets gameState ++ [(smallBlindPlayer, 5)]}

            bigBlindPlayer = (players gameState !! bigBlindPosition smallBlindGameState)
                {chips = chips (players smallBlindGameState !! bigBlindPosition gameState) - 10}
            bigBlindGameState = smallBlindGameState{players = updatePlayerList (players smallBlindGameState) bigBlindPlayer,
            pot = pot smallBlindGameState + 10, bets = bets smallBlindGameState ++ [(bigBlindPlayer, 10)]}
        in bigBlindGameState

    {-
        Rotates the position of the dealer, small and big blind for the active players

        Small and big blind positions are incremented and `mod` is used to make sure the
        positions return back to the start of the list if the position goes over
        
        Big blind position is always +1 of the small blind position to make sure that
        if the big blind goes bust a player is not simultaneously both blinds
    -}
    rotatePositions :: GameState -> Int -> GameState
    rotatePositions gameState newDealerPos =
        let totalActivePlayers = length (activePlayersList gameState)
            updatedDealerPosition = newDealerPos

            updatedSmallBlindPosition = (updatedDealerPosition + 1) `mod` totalActivePlayers

            updatedBigBlindPosition = (updatedSmallBlindPosition + 1) `mod` totalActivePlayers

            updatedGameState = gameState{dealerPosition = updatedDealerPosition,
            smallBlindPosition = updatedSmallBlindPosition, bigBlindPosition = updatedBigBlindPosition}
        in updatedGameState

    {-
        Sets the dealer from the active players and updates game state
    -}
    setDealer :: GameState -> GameState
    setDealer gameState =
        let
            activePlayers = activePlayersList gameState
            dealer = (activePlayers !! dealerPosition gameState){isDealer = True}
            updatedGameState = gameState{players = updatePlayerList (players gameState) dealer}
        in updatedGameState

    {-
        Resets the dealer and updates game state
    -}
    resetDealer :: GameState -> GameState
    resetDealer gameState =
        let updatedPlayers = [p {isDealer = False} | p <- players gameState]
            updatedGameState = gameState {players = updatedPlayers}
        in updatedGameState
    
    {-
        Resets all the players active boolean to true
    -}
    resetActivePlayers :: GameState -> GameState
    resetActivePlayers gameState =
        let updatedPlayers = [p {isActive = True} | p <- players gameState]
            updatedGameState = gameState {players = updatedPlayers}
        in updatedGameState
    

    {-
        Any players with less chips than the big blind are removed
        from the players list
    -}
    removeBustPlayers :: GameState -> GameState
    removeBustPlayers gameState =
        let updatedPlayers = [p | p <- players gameState, chips p >= 10]
            updatedGameState = gameState {players = updatedPlayers}
        in updatedGameState

    {-
        Sets bets to be empty
    -}
    resetBets :: GameState -> GameState
    resetBets gameState = gameState{bets = []}

    {-
        Updates the winners chips by distributing the chips equally
        and empty the pot

        If only one winner then they would get all the chips
    -}
    distributeWinnings :: [Player] -> GameState -> GameState
    distributeWinnings winners gameState = do
        let updatedWinners = [p{chips = chips p + pot gameState `div` length winners} | p <- winners]
            updatedPlayerList = foldl updatePlayerList (players gameState) updatedWinners
            updatedGameState = gameState {players = updatedPlayerList, pot = 0}
            in updatedGameState
    
    {-
        Reset the community cards to be empty
    -}
    clearComCards :: GameState -> GameState
    clearComCards gameState = gameState{communityCards = []}

    {-
        Calculates the next dealer position by finding the next active
        player to the left of the current dealer and getting their index from the
        removed bust player list
    -}
    findNextDealerPosition :: [Player] -> [Player] -> Int -> Int
    findNextDealerPosition oldPlayers newPlayers oldDealerPos
        -- If the dealer was the last player on the list return 0 (to wrap around)
        | oldDealerPos == length oldPlayers - 1 = 0
        | otherwise = 
            let oldPlayersDrop = drop (oldDealerPos+1) oldPlayers
                -- Finds all non-bust players to the left of the dealer
                validNextPlayers = [p | p <- oldPlayersDrop, chips p >=10]
            in if null validNextPlayers
                then 0
                else fromMaybe
                -- Returns the position of the first non-bust player in the new list
                0 (elemIndex (head validNextPlayers) newPlayers) 
            


    






    -- Player Behaviour Functions

    {-
        Chooses a random index for the valid player actions
        and pick the action at that point
    -}
    randomPlayerAction :: [PlayerAction] -> StdGen -> (PlayerAction, StdGen)
    randomPlayerAction validActions gen =
        let (index, newGen) = randomR (0, length validActions - 1) gen
        in (validActions !! index, newGen)

    {-
        Filters out the raise action and then performs the random
        action selection
    -}
    passivePlayerAction :: [PlayerAction] -> StdGen -> (PlayerAction, StdGen)
    passivePlayerAction validActions gen =
        let updatedActions = filter (/= Raise) validActions
            (index, newGen) = randomR (0, length updatedActions - 1) gen
        in (updatedActions !! index, newGen)

    {-
        If the player has the valid action to bet/raise then these actions
        will double in the list meaning when the random action is picked
        there is a higher liklihood of it being a bet/raise    
    -}
    aggressivePlayerAction :: [PlayerAction] -> StdGen -> (PlayerAction, StdGen)
    aggressivePlayerAction validActions gen 
        | validActions == [Fold] = (Fold, gen)
        | validActions == [Fold, Check] = (Check, gen)
        | otherwise =
            let updatedActions 
                    -- Doubles raise
                    | Raise `elem` validActions = 
                        concat [if a == Raise then [a, a] else [a] | a <- validActions]
                    -- Doubles bet
                    | Bet `elem` validActions = 
                        concat [if a == Bet then [a, a] else [a] | a <- validActions]
                    | otherwise = validActions
                (index, newGen) = randomR (0, length updatedActions - 1) gen
            in (updatedActions !! index, newGen)

    {-
        Finds the optimal list of actions based upon how good the players best hand is
        and if the players hand is poor the pot odds of the game for the player will be 
        evaluated to find the optimal list of actions
    -}
    smartPlayerAction :: [PlayerAction] -> Player -> GameState -> StdGen -> (PlayerAction, StdGen)
    smartPlayerAction validActions player gameState gen
        | validActions == [Fold] = (Fold, gen)
        | validActions == [Call] = (Call, gen)
        | validActions == [Fold, Call] = (Call, gen)
        | validActions == [Fold, Check] = (Check, gen)
        | otherwise =
            let maxHand = evaluateCombinationHands (hand player ++ communityCards gameState)
                potOdd = potOdds player gameState
                score = scoreEvaluateHand maxHand
                updatedActions
                    -- Strong hand
                    | score >= 7 = filter (/= Call) (filter (/= Check) (filter (/= Fold) validActions)) 
                    -- Okay hand
                    | score >= 4 =  filter (/= Check) (filter (/= Fold) validActions) 
                    -- Poor hand
                    | score >= 2 = filter (/= Bet) (filter (/= Raise) validActions)
                    -- Good pot odds
                    | potOdd >= 4 = filter (/= Fold) validActions
                    -- Okay pot odds
                    | potOdd >= 2 = filter (/= Fold) validActions
                    | otherwise = validActions
                (index, newGen) = randomR (0, length updatedActions - 1) gen
            in (updatedActions !! index, newGen)

    {-
        Provides a score based on the hand rank given
        Does not focus on kickers and high cards, only on specific the hand rank
    -}
    scoreEvaluateHand :: HandRank -> Int
    scoreEvaluateHand hand = case hand of
        RoyalFlush           -> 10
        StraightFlush _      -> 9
        FourKind _ _         -> 8
        FullHouse _ _        -> 7
        Flush _              -> 6
        Straight _           -> 5
        ThreeKind _ _        -> 4
        TwoPair _ _ _        -> 3
        Pair _ _             -> 2
        HighCard _ _         -> 1

    {-
        Calculates the ratio of the pot size to the amount you needed to call for the player   
    -}
    potOdds :: Player -> GameState -> Double
    potOdds player gameState =
        let potSize = pot gameState
        -- Gets call amount
            callAmount = totalBetSum (bets gameState) - playerBetSum player (bets gameState)
        in if callAmount == 0 
                then 0
                -- Calculates pot odd
                else (fromIntegral potSize) / (fromIntegral callAmount)
        







    -- Additional Information Functions

    {-
        Gets the rank and suit of the card and converts it to a string    
    -}
    showCard :: Card -> String
    showCard (Card r s) = show r ++ " of " ++ show s

    {-
        Additional Information to get a string of cards (with their rank and suit)
        from a list of cards
    -}
    showCards :: [Card] -> String
    showCards cards = unwords (map showCard cards)

    {-
        gets the name of a specific player    
    -}
    showPlayer :: Player -> String
    showPlayer p = name p

    {-
        Additional Information to get a string of the names of the players
        given a list of players
    -}
    showPlayers :: [Player] -> String
    showPlayers players = unwords (map showPlayer players)

    {-
        Additional information of the players hands and their best 5 card hand
        of the active players
    -}
    printPlayerHands :: GameState -> IO ()
    printPlayerHands gameState = do
        putStrLn "Community Cards:"
        putStrLn (showCards (communityCards gameState))
        putStrLn ""
        let activePlayers = activePlayersList gameState
        printPlayers activePlayers
        where
            printPlayers :: [Player] -> IO ()
            printPlayers [] = return ()
            printPlayers (player:ps) = do
                let fullHand = hand player ++ communityCards gameState
                let handRank = evaluateCombinationHands fullHand
                putStrLn "-------------------------"
                putStrLn ("Player: " ++ name player)
                putStrLn ("Combined Hand: " ++ showCards fullHand)
                putStrLn ("Hand Rank: " ++ show handRank)
                putStrLn "-------------------------"
                printPlayers ps

    {-
        Additional information for a players turn
        displaying the player, their action, the chips they bet, the chips they have left
        and the pot value
    -}
    additionalInfoPlayerAction :: Player -> Player -> PlayerAction -> GameState -> IO()
    additionalInfoPlayerAction oldPlayer newPlayer action gameState = do
        putStrLn  ""
        putStrLn $ "Player: " ++ name newPlayer 
        putStrLn $ "Action: " ++ show action ++ ", Chips Bet This Turn: " ++ show 
            (chips oldPlayer - chips newPlayer)
            ++ ", Chips Left: " ++ show(chips newPlayer)
        putStrLn $ "Pot: " ++ show (pot gameState)
        putStrLn  ""





    --Main function used to start program

    main :: IO ()
    main = do
        gen <- newStdGen
        let deck = generateDeck -- Generate deck
        let player1 = Player "Random Player" [] 1000 False Random True
        let player2 = Player "Passive Player" [] 1000 False Passive True
        let player3 = Player "Aggressive Player" [] 1000 False Aggressive True
        let player4 = Player "Smart Player" [] 1000 False Smart True
        let playersList = [player1, player2, player3, player4] -- Initialize players
        let gameState = GameState playersList deck [] 0 [] 0 1 2


        (gs, newGen) <- gameLoop gameState gen 0
        print(players gs)


        
