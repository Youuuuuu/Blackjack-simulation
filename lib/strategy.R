##############################################
# Simple strategy
##############################################

strategy_simple <- function(mine, dealerFaceUp) {
    if (handValue(mine) == 0)
        return ("S")
    if (handValue(dealerFaceUp) > 6 && handValue(mine) < 17)
        "H"
    else
        "S"
}

##############################################
# Optimal strategy
# ##############################################

strategy_optimal <- function(player_hand, dealerFaceUp,
                             optimal = lookuptable) {
    # Stand if 21 or already busted
    player_value <- handValue(player_hand)
    if (player_value == 0)
        return("S")
    if (player_value >= 21)
        return("S")
    dealer_value <- handValue(dealerFaceUp)
    
    loc_ace <- player_hand == 1
    
    if (length(player_hand) == 2 &&
        player_hand[1] == player_hand[2]) {
        type <- 'pair'
        if (player_hand[1] == 1)
            player_value <- 2
    } else if (sum(loc_ace) > 0 &&
               (player_value - sum(loc_ace)) > handValue(player_hand[!loc_ace])) {
        type <- "soft"
    } else {
        type <- "hard"
    }
    
    out <- optimal[optimal$type == type &
                       optimal$value == player_value,
                   as.character(dealer_value)]
    if (length(out) == 0) stop("Optimal value not found. Check lookupTable.")

    if (out == "Dh")
        if (length(player_hand) > 2) out = "H" else out = "D"
    if (out == "Ds" )
        if (length(player_hand) > 2) out = "S" else out = "D"


    out
}

