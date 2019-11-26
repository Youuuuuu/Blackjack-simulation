#  Create a deck
deck <- rep(c(1:10, 10, 10, 10), 4)

# Simple shuffling of deck
shuffle_decks <- function(n) sample(rep(deck, n))

# Calculate hand value
handValue <- function(cards){
    value <- sum(cards)
    # Check for an Ace and change value if it doesn't bust
    if (any(cards == 1) && value <= 11)
        value <- value + 10
    # Check bust (set to 0); check black jack (set to 21.5)
    if (value > 21)
        0
    else if (value == 21 && length(cards) == 2)
        21.5 # Blackjack
    else
        value
}

# Calculate win - player against dealer
winnings <- function(dealer, players){
    if (dealer == 21.5) {
        # Dealer has blackjack, ties players with blackjack
        -1 * (players <= 21)    
    } else if (dealer == 0) {
        # Dealer busts - all non-busted palyers win
        1.5 * (players == 21.5) +
        1 * (players <= 21 & players > 0) +
        -1 * (players == 0)
    } else
        # Dealer 21 or below, all player values > dealer win
        1.5 * (players == 21.5) +
        1 * (players <= 21 & players > dealer) +
        -1 * (players <= 21 & players < dealer)
}

# Create a deck of cards
shoe <- function(m=1) sample(deck, m, replace=TRUE)

# Get two cards - game start
new_hand <- function(shoe, cards=shoe(2), bet=1){
    structure(list(bet=bet, shoe=shoe, cards=cards), class="hand")
}


print.hand <- function(x, ...) {
    cat("Blackjack hand: ", paste(x$cards, collapse = "-"),
        " (", handValue(x$cards), "). Bet: ", x$bet,
        "\n", sep="")
}

# Draw one card
hit <- function(hand, shoe=shoe) {
    hand$cards <- c(hand$cards, hand$shoe(1))
    hand
}

# Double Drown - double the bet and draw one card
dd <- function(hand) {
    hand$bet <- hand$bet * 2
    hand <- hit(hand)
    stand(hand)
}

# Do nothing - STAND
stand <- function(hand) hand

# Split pair into two hands
splitPair <- function(hand) {
    if (hand$cards[1] != hand$cards[2]) {
        stop("Cannot split. Cards are not identical")
    }
    else if (length(hand$cards)>2) {
        stop(paste(length(hand$cards), "Cannot split after hit."))
    }
    else{
        list(
            new_hand(
                hand$shoe,
                cards <- c(hand$cards[1], hand$shoe(1)),
                bet <- hand$bet
            ),
            new_hand(
                hand$shoe,
                cards <- c(hand$cards[2], hand$shoe(1)),
                bet <- hand$bet
            )
        )
    }
}

# Dealer - drawing cards
dealer_cards <- function(shoe) {
    cards <- shoe(2)
    while (handValue(cards) < 17 && handValue(cards) > 0) {
        cards <- c(cards, shoe(1))
    }
    cards
}

# GAME FUNCTION
play_hand <- function(shoe, strategy,
                      hand=new_hand(shoe),
                      dealer=dealer_cards(shoe),
                      verbose=FALSE){
    
    if (verbose) {
        cat("New hand \n")
        cat("  Dealer: ", paste(dealer, collapse='-'),
            " (", handValue(dealer), ")\n", sep="")
        cat("  Player: ", paste(hand$cards, collapse="-"),
            ": ", sep="", "\n")
    }
    
    face_card <- dealer[1]
    action <- strategy(hand$cards, face_card)
    while (action != "S" && handValue(hand$cards) != 0) {
        if (verbose) {
            cat("  ", action, " -> ", paste(hand$cards, collapse="-"),
                " (", handValue(hand$cards), ")", sep="", "\n")
        }
        if (action == "H") {
            hand <- hit(hand)
            action <- strategy(hand$cards, face_card)
        } else if (action == "D") {
            hand <- dd(hand)
            action <- "S"
        } else if (action == "SP") {
            hand <- splitPair(hand)
            action <- "S"
        } else {
            stop(paste("Unknown action: ", "<<", action, ">> should be one of S, H, D, SP"))
        }
        if (verbose) {
            cat("  ", action, " -> ", paste(hand$cards, collapse="-"),
                " (", handValue(hand$cards), ")", sep="", "\n")
        }
    }
    if (length(hand)==2){
        hand_result1 <- winnings(handValue(dealer), handValue(hand[[1]]$cards)) * hand[[1]]$bet
        hand_result2 <- winnings(handValue(dealer), handValue(hand[[2]]$cards)) * hand[[2]]$bet
        hand_result <- hand_result1 + hand_result2
    } else {
        hand_result <- winnings(handValue(dealer), handValue(hand$cards)) * hand$bet
    }
    if (verbose) {
        cat("Hand result: ", hand_result, "\n\n")
    }
    hand_result
}