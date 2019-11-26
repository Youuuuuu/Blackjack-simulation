library(ProjectTemplate)
load.project()


# TRY GAME (set seed)
set.seed(1014)

dealer <- new_hand(shoe)
player <- new_hand(shoe)

# One of dealer's cards is visible
# dealer$cards[1]
s0 <- paste("Dealers card: ", dealer$cards[1])
s1 <- paste("Player's hand: ", player$cards[1], "-", player$cards[2])

player <- hit(player) # player choose to hit and get 1 cards
s2 <- paste("Player choose to hit and end up with : ", player$cards)

# dealer reveals his cards
s3 <- paste("Dealer's both cards were: ", dealer$cards)
dealer <- hit(dealer) # dealer has to hit if value is less than 17
s4 <- paste("Dealer's handValue after required hitting: ", dealer)

wins <- winnings(handValue(dealer$cards), handValue(player$cards))
s5 <- paste("Winnings: ", wins)

# file.remove("reports/step_wise.txt")
write(paste(s0, s1, s2, s3, s4, s5, sep='\n'), 'reports/step_wise.txt')
# write(s2, 'reports/step_wise.txt', append=TRUE)
# write(s3, 'reports/step_wise.txt', append=TRUE)
# write(s4, 'reports/step_wise.txt', append=TRUE)